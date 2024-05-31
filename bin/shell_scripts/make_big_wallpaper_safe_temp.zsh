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
    local output_dir="$1"
    mkdir -p "$output_dir"
}

# Get image dimensions using ImageMagick
get_image_dimensions() {
    local image="$1"
    magick identify -format "%wx%h" "$image"
}

# Calculate the aspect ratio
calculate_aspect_ratio() {
    local width="$1"
    local height="$2"
    echo "scale=2; $width / $height" | bc
}

# Apply Super Resolution workflow
apply_super_resolution() {
    local input_image="$1"
    automator -i "$input_image" "$SUPER_RES_WORKFLOW"
}

# Resize the image while maintaining the aspect ratio
resize_image() {
    local input_image="$1"
    local resized_image="$2"
    local dimension="$3"
    magick "$input_image" -filter Lanczos -resize "$dimension" "$resized_image"
}

# Verify if the super resolution workflow generated the expected output
verify_super_resolution_output() {
    local super_res_image="$1"
    if [ ! -f "$super_res_image" ]; then
        error "Super resolution workflow did not generate the expected output for $super_res_image."
        return 1
    fi
}

# Apply Convert to HEIC workflow
apply_convert_heic() {
    local resized_image="$1"
    automator -i "$resized_image" "$CONVERT_HEIC_WORKFLOW"
}

# Clean up temporary resized images
cleanup_temp_files() {
    local temp_files=("$@")
    for file in "${temp_files[@]}"; do
        rm "$file"
    done
}

# Handle Super Resolution process
handle_super_resolution() {
    local input_image="$1"
    local resized_image="$2"
    local super_res_image="$3"
    local orig_error_dir="$4"
    local target_min_dim="$5"
    local target_max_dim="$6"
    local resize_dim="$7"
    local dim_index="$8"

    info "Applying Super Resolution to $(basename "$input_image")..."
    apply_super_resolution "$input_image"

    if ! verify_super_resolution_output "$super_res_image"; then
        return 1
    fi

    local super_res_dim
    super_res_dim=$(get_image_dimensions "$super_res_image" | cut -d'x' -f"$dim_index")

    if [ "$super_res_dim" -lt "$target_min_dim" ]; then
        error "$(basename "$super_res_image") is still not large enough after applying Super Resolution."
        mv "$input_image" "$orig_error_dir"
        cleanup_temp_files "$super_res_image"
        return 1
    fi

    info "$(basename "$super_res_image") is large enough after applying Super Resolution. Continuing..."

    if [ "$super_res_dim" -ge "$target_max_dim" ]; then
        info "Downscaling $(basename "$super_res_image") to target size..."
        resize_image "$super_res_image" "$resized_image" "$resize_dim"
    else
        info "Renaming $(basename "$super_res_image") to $(basename "$resized_image")"
        cp "$super_res_image" "$resized_image"
    fi

    cleanup_temp_files "$super_res_image"
}

# Generic function to process image
create_image() {
    local input_image="$1"
    local resized_image="$2"
    local super_res_image="$3"
    local orig_error_dir="$4"
    local image_dim="$5"
    local target_min_dim="$6"
    local target_max_dim="$7"
    local resize_dim="$8"
    local dim_index="$9"

    local target_output_dim=$((image_dim * 3))

    if (( target_output_dim < target_min_dim )); then
        error "$(basename "$input_image") will not be large enough even after applying Super Resolution."
        mv "$input_image" "$orig_error_dir"
        return 1
    fi

    if [ "$image_dim" -ge "$target_max_dim" ]; then
        info "$(basename "$input_image") is already large enough. Skipping Super Resolution..."
        resize_image "$input_image" "$resized_image" "$resize_dim"
    else
        handle_super_resolution "$input_image" "$resized_image" "$super_res_image" "$orig_error_dir" "$target_min_dim" "$target_max_dim" "$resize_dim" "$dim_index"
    fi
}

# Process a single image
process_image() {
    local input_image="$1"
    local basename=$(basename "$input_image" | sed 's/\.[^.]*$//')
    local orig_store_dir="$HOME/Pictures/wallpaper_originals"
    local orig_error_dir="$HOME/Pictures/wallpaper_error"
    local super_res_output="$HOME/Pictures/wallpaper/safe"
    local super_res_image="$super_res_output/${basename}_super_res_output.png"
    local resized_image="$super_res_output/${basename}_ml_res_wallpaper.png"

    create_output_directory "$super_res_output"
    create_output_directory "$orig_store_dir"
    create_output_directory "$orig_error_dir"

    local dimensions
    dimensions=$(get_image_dimensions "$input_image")
    local image_width=${dimensions%x*}
    local image_height=${dimensions#*x}

    local aspect_ratio
    aspect_ratio=$(calculate_aspect_ratio "$image_width" "$image_height")

    if (( $(echo "$aspect_ratio <= 1.6" | bc -l) )); then
        info "$(basename "$input_image") aspect ratio is less than or equal to 16:10. Resizing based on width..."
        create_image "$input_image" "$resized_image" "$super_res_image" "$orig_error_dir" "$image_width" 5120 7680 "7680x" 1
    else
        info "$(basename "$input_image") aspect ratio is greater than 16:10. Resizing based on height..."
        create_image "$input_image" "$resized_image" "$super_res_image" "$orig_error_dir" "$image_height" 3200 4800 "x4800" 2
    fi

    apply_convert_heic "$resized_image"
    success "Processing complete for $input_image."
    mv "$input_image" "$orig_store_dir"
    echo
}

# Check if the input image or directory is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <input-image-or-directory>"
    exit 1
fi

input_path="$1"

# Check if the input is a directory
if [ -d "$input_path" ]; then
    for image in "$input_path"/*; do
        if [ -f "$image" ]; then
            process_image "$image"
        fi
    done
elif [ -f "$input_path" ]; then
    process_image "$input_path"
else
    echo "Error: $input_path is neither a file nor a directory."
    exit 1
fi
