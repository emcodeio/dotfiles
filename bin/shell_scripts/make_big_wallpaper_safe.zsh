#!/usr/bin/env zsh

# Define colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Function to wrap text in blue
info() {
    echo -e "${BLUE}$1${NC}"
}

# Function to wrap text in red
error() {
    echo -e "${RED}$1${NC}"
}

# Function to wrap text in green
success() {
    echo -e "${GREEN}$1${NC}"
}

# Define the paths to the Automator workflows
SUPER_RES_WORKFLOW="$HOME/.dotfiles/bin/automator_workflows/super_res_wallpaper_safe.workflow"
CONVERT_HEIC_WORKFLOW="$HOME/.dotfiles/bin/automator_workflows/convert_to_heic.workflow"

# Ensure output directory exists
create_output_directory() {
    local OUTPUT_DIR="$1"
    mkdir -p "$OUTPUT_DIR"
}

# Get image width using ImageMagick
get_image_width() {
    local IMAGE="$1"
    magick identify -format "%w" "$IMAGE"
}

# Get image height using ImageMagick
get_image_height() {
    local IMAGE="$1"
    magick identify -format "%h" "$IMAGE"
}

# Calculate the aspect ratio
calculate_aspect_ratio() {
    local WIDTH="$1"
    local HEIGHT="$2"
    echo "scale=2; $WIDTH / $HEIGHT" | bc
}

# Apply Super Resolution workflow
apply_super_resolution() {
    local INPUT_IMAGE="$1"
    automator -i "$INPUT_IMAGE" "$SUPER_RES_WORKFLOW"
}

# Resize the image to a height of 4800 pixels while maintaining the aspect ratio
resize_image_by_width() {
    local INPUT_IMAGE="$1"
    local RESIZED_IMAGE="$2"
    magick "$INPUT_IMAGE" -filter Lanczos -resize 7680x "$RESIZED_IMAGE"
}

# Resize the image to a height of 4800 pixels while maintaining the aspect ratio
resize_image_by_height() {
    local INPUT_IMAGE="$1"
    local RESIZED_IMAGE="$2"
    magick "$INPUT_IMAGE" -filter Lanczos -resize x4800 "$RESIZED_IMAGE"
}

# Verify if the super resolution workflow generated the expected output
verify_super_resolution_output() {
    local SUPER_RES_IMAGE="$1"
    if [ ! -f "$SUPER_RES_IMAGE" ]; then
        error "Super resolution workflow did not generate the expected output for $SUPER_RES_IMAGE."
        return 1
    fi
}

# Apply Convert to HEIC workflow
apply_convert_heic() {
    local RESIZED_IMAGE="$1"
    automator -i "$RESIZED_IMAGE" "$CONVERT_HEIC_WORKFLOW"
}

# Clean up temporary resized images
cleanup_temp_files() {
    local TEMP_FILES=("$@")
    for FILE in "${TEMP_FILES[@]}"; do
        rm "$FILE"
    done
}

# Process a single image
process_image() {
    local INPUT_IMAGE="$1"
    local BASENAME=$(basename "$INPUT_IMAGE" | sed 's/\.[^.]*$//')
    local ORIG_STORE_DIR="$HOME/Pictures/wallpaper_originals"
    local ORIG_ERROR_DIR="$HOME/Pictures/wallpaper_error"
    local SUPER_RES_OUTPUT="$HOME/Pictures/wallpaper/safe"
    local SUPER_RES_IMAGE="$SUPER_RES_OUTPUT/${BASENAME}_super_res_output.png"
    local RESIZED_IMAGE="$SUPER_RES_OUTPUT/${BASENAME}_ml_res_wallpaper.png"

    create_output_directory "$SUPER_RES_OUTPUT"
    create_output_directory "$ORIG_STORE_DIR"
    create_output_directory "$ORIG_ERROR_DIR"

    local IMAGE_WIDTH
    local IMAGE_HEIGHT
    IMAGE_WIDTH=$(get_image_width "$INPUT_IMAGE")
    IMAGE_HEIGHT=$(get_image_height "$INPUT_IMAGE")

    local ASPECT_RATIO
    ASPECT_RATIO=$(calculate_aspect_ratio "$IMAGE_WIDTH" "$IMAGE_HEIGHT")

    if (( $(echo "$ASPECT_RATIO <= 1.6" | bc -l) )); then

        info "$(basename "$INPUT_IMAGE") apsect ratio is is less than or equal to 16:10. Apply resizing based on width..."

        local TARGET_OUTPUT_WIDTH=IMAGE_WIDTH*3

        if (( TARGET_OUTPUT_WIDTH < 5120 )); then
            error "$(basename "$INPUT_IMAGE") will be not be wide enough even after apply Super Resolution."
            error "Projected resolution width is $TARGET_OUTPUT_WIDTH pixels."
            error "Moving $(basename $INPUT_IMAGE) to $ORIG_ERROR_DIR..."
            mv "$INPUT_IMAGE" "$ORIG_ERROR_DIR"
            return 1
        fi

        if [ "$IMAGE_WIDTH" -ge 7680 ]; then
            info "Skipping Super Resolution. $(basename "$INPUT_IMAGE") is already large enough..."
            resize_image_by_width "$INPUT_IMAGE" "$RESIZED_IMAGE"
        else
            info "Applying Super Resolution. $(basename "$INPUT_IMAGE") not large enough..."
            apply_super_resolution "$INPUT_IMAGE"

            if ! verify_super_resolution_output "$SUPER_RES_IMAGE"; then
                return 1
            fi

            local SUPER_RES_WIDTH
            SUPER_RES_WIDTH=$(get_image_width "$SUPER_RES_IMAGE")

            if [ "$SUPER_RES_WIDTH" -lt 5120 ]; then
                error "$(basename "$SUPER_RES_IMAGE") is still not large enough after applying Super Resolution."
                error "Resolution width is $SUPER_RES_WIDTH pixels."
                error "Moving $(basename $INPUT_IMAGE) to $ORIG_ERROR_DIR..."
                mv "$INPUT_IMAGE" "$ORIG_ERROR_DIR"
                cleanup_temp_files "$SUPER_RES_IMAGE"
                return 1
            fi

            if [ "$SUPER_RES_WIDTH" -ge 7680 ]; then
                info "Downscaling $(basename "$SUPER_RES_IMAGE") to 8K..."
                resize_image_by_width "$SUPER_RES_IMAGE" "$RESIZED_IMAGE"
            fi
            cleanup_temp_files "$SUPER_RES_IMAGE"
        fi
    else

        info "$(basename "$INPUT_IMAGE") apsect ratio is is greater than 16:10. Apply resizing based on height..."

        local TARGET_OUTPUT_HEIGHT=IMAGE_HEIGHT*3

        if (( TARGET_OUTPUT_HEIGHT < 3200 )); then
            error "$(basename "$INPUT_IMAGE") will not be tall enough even after apply Super Resolution."
            error "Projected resolution height is $TARGET_OUTPUT_HEIGHT pixels."
            error "Moving $(basename $INPUT_IMAGE) to $ORIG_ERROR_DIR..."
            mv "$INPUT_IMAGE" "$ORIG_ERROR_DIR"
            return 1
        fi

        if [ "$IMAGE_HEIGHT" -ge 4800 ]; then
            info "Skipping Super Resolution. $(basename "$INPUT_IMAGE") is already large enough..."
            resize_image_by_height "$INPUT_IMAGE" "$RESIZED_IMAGE"
        else
            info "Applying Super Resolution. $(basename "$INPUT_IMAGE") not large enough..."
            apply_super_resolution "$INPUT_IMAGE"

            if ! verify_super_resolution_output "$SUPER_RES_IMAGE"; then
                return 1
            fi

            local SUPER_RES_HEIGHT
            SUPER_RES_HEIGHT=$(get_image_height "$SUPER_RES_IMAGE")

            if [ "$SUPER_RES_HEIGHT" -lt 3200 ]; then
                error "$(basename "$SUPER_RES_IMAGE") is still not large enough after applying Super Resolution."
                error "Resolution height is $SUPER_RES_WIDTH. Exiting..."
                info "Moving $(basename $INPUT_IMAGE) to $ORIG_ERROR_DIR..."
                mv "$INPUT_IMAGE" "$ORIG_ERROR_DIR"
                cleanup_temp_files "$SUPER_RES_IMAGE"
                return 1
            fi

            if [ "$SUPER_RES_WIDTH" -ge 4800 ]; then
                info "Downscaling $(basename "$SUPER_RES_IMAGE") to 8K..."
                resize_image_by_height "$SUPER_RES_IMAGE" "$RESIZED_IMAGE"
            fi

            cleanup_temp_files "$SUPER_RES_IMAGE"
        fi
    fi

    apply_convert_heic "$RESIZED_IMAGE"
    info "Processing complete for $INPUT_IMAGE."
    info "Moving $(basename $INPUT_IMAGE) to $ORIG_STORE_DIR..."
    mv "$INPUT_IMAGE" "$ORIG_STORE_DIR"
    echo
}

# Check if the input image or directory is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <input-image-or-directory>"
    exit 1
fi

INPUT_PATH="$1"

# Check if the input is a directory
if [ -d "$INPUT_PATH" ]; then
    # Iterate over each file in the directory
    for IMAGE in "$INPUT_PATH"/*; do
        if [ -f "$IMAGE" ]; then
            process_image "$IMAGE"
        fi
    done
elif [ -f "$INPUT_PATH" ]; then
    # Process the single image file
    process_image "$INPUT_PATH"
else
    echo "Error: $INPUT_PATH is neither a file nor a directory."
    exit 1
fi
