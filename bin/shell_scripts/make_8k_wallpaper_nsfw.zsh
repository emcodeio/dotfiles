#!/usr/bin/env zsh

# Define the paths to the Automator workflows
SUPER_RES_WORKFLOW="$HOME/.dotfiles/bin/automator_workflows/super_res_wallpaper_nsfw.workflow"
CONVERT_HEIC_WORKFLOW="$HOME/.dotfiles/bin/automator_workflows/convert_to_heic.workflow"

# Ensure output directory exists
create_output_directory() {
    local OUTPUT_DIR="$1"
    mkdir -p "$OUTPUT_DIR"
}

# Apply Super Resolution workflow
apply_super_resolution() {
    local INPUT_IMAGE="$1"
    automator -i "$INPUT_IMAGE" "$SUPER_RES_WORKFLOW"
}

# Verify if the super resolution workflow generated the expected output
verify_super_resolution_output() {
    local SUPER_RES_IMAGE="$1"
    if [ ! -f "$SUPER_RES_IMAGE" ]; then
        echo "Super resolution workflow did not generate the expected output for $SUPER_RES_IMAGE."
        return 1
    fi
}

# Resize the image to a width of 7680 pixels while maintaining the aspect ratio
resize_image() {
    local INPUT_IMAGE="$1"
    local RESIZED_IMAGE="$2"
    magick "$INPUT_IMAGE" -filter Lanczos -resize 7680x "$RESIZED_IMAGE"
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

# Check image width
get_image_width() {
    local IMAGE="$1"
    magick identify -format "%w" "$IMAGE"
}

# Process a single image
process_image() {
    local INPUT_IMAGE="$1"
    local BASENAME=$(basename "$INPUT_IMAGE" | sed 's/\.[^.]*$//')
    local SUPER_RES_OUTPUT="$HOME/Pictures/wallpaper/nsfw"
    local SUPER_RES_IMAGE="$SUPER_RES_OUTPUT/${BASENAME}_super_res_output.png"
    local RESIZED_IMAGE="$SUPER_RES_OUTPUT/${BASENAME}_super_res_wallpaper.png"

    create_output_directory "$SUPER_RES_OUTPUT"

    local IMAGE_WIDTH
    IMAGE_WIDTH=$(get_image_width "$INPUT_IMAGE")

    if [ "$IMAGE_WIDTH" -le 7680 ]; then
        apply_super_resolution "$INPUT_IMAGE"

        if ! verify_super_resolution_output "$SUPER_RES_IMAGE"; then
            return 1
        fi

        resize_image "$SUPER_RES_IMAGE" "$RESIZED_IMAGE"
        cleanup_temp_files "$SUPER_RES_IMAGE"
    else
        resize_image "$INPUT_IMAGE" "$RESIZED_IMAGE"
    fi

    apply_convert_heic "$RESIZED_IMAGE"
    echo "Processing complete for $INPUT_IMAGE."
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
