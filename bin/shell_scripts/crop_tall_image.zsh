#!/usr/bin/env zsh

# Check if the input image is provided as an argument
if [ $# -eq 0 ]; then
    echo "No input image provided. Usage: ./crop_image.sh <input_image>"
    exit 1
fi

# Input image from the command line argument
input_image="$1"

# Output image filenames based on the input image name
output_image_top="${input_image%.*}_top.${input_image##*.}"
output_image_middle="${input_image%.*}_middle.${input_image##*.}"
output_image_bottom="${input_image%.*}_bottom.${input_image##*.}"

# Get the image dimensions
width=$(magick identify -format "%w" "$input_image")
height=$(magick identify -format "%h" "$input_image")

# Check if the image height is sufficient for a 16:10 aspect ratio
if (( height / width < 5 / 4 )); then
    echo "Image is too short to crop three times. Finishing..."
    exit 1
fi

echo "Image is tall enough to crop three times. Cropping..."

# Calculate the height for a 16:10 aspect ratio
crop_height=$((width * 10 / 16))

# Calculate the starting points for each crop
start_top=0
start_middle=$(((height / 2) - (crop_height / 2)))
start_bottom=$((height - crop_height))

# Crop the top part
magick "$input_image" -crop "${width}x${crop_height}+0+${start_top}" "$output_image_top"

# Crop the middle part
magick "$input_image" -crop "${width}x${crop_height}+0+${start_middle}" "$output_image_middle"

# Crop the bottom part
magick "$input_image" -crop "${width}x${crop_height}+0+${start_bottom}" "$output_image_bottom"

echo "Crops completed: $output_image_top, $output_image_middle, $output_image_bottom"
