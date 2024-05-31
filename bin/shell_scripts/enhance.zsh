#!/usr/bin/env zsh

# Define the paths to the Automator workflows
SUPER_RES_WORKFLOW="$HOME/.dotfiles/bin/automator_workflows/ml_enhance.workflow"

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

# Process a single image
process_image() {
    local INPUT_IMAGE="$1"
    local BASENAME=$(basename "$INPUT_IMAGE" | sed 's/\.[^.]*$//')
    local SUPER_RES_OUTPUT="$HOME/Pictures/ml_enhanced"
    # local SUPER_RES_IMAGE="$SUPER_RES_OUTPUT/${BASENAME}_super_res_output.png"
    # local RESIZED_IMAGE="$SUPER_RES_OUTPUT/${BASENAME}_super_res_wallpaper.png"

    create_output_directory "$SUPER_RES_OUTPUT"

    apply_super_resolution "$INPUT_IMAGE"

    echo "Enhancing $INPUT_IMAGE resolution complete."
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
