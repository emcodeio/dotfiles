#!/usr/bin/env python3

import base64
import requests
import os
from dotenv import load_dotenv
import sys

# Load environment variables
load_dotenv()

# OpenAI API Key
OPENAI_API_KEY = os.environ.get("OPENAI_API_KEY_WORK", "<your OpenAI API key if not set as env var>")

# Function to encode the image
def encode_image(image_path):
    with open(image_path, "rb") as image_file:
        return base64.b64encode(image_file.read()).decode('utf-8')

# Function to get image name from GPT
def get_image_name(base64_image):
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {OPENAI_API_KEY}"
    }

    payload = {
        "model": "gpt-4o",
        "messages": [
            {
                "role": "user",
                "content": [
                    {
                        "type": "text",
                        "text": "You are part of a python script made for renaming images based on what they contain. Please provide a good name for this image. Please stick to standard file name formatting, such as using underscores instead of spaces, no uppercase letters, etc. Only provide the file name with no explanation or extra wording. Your response will be used to literally rename the image automatically. It's critical that you only give the suggested image file name."
                    },
                    {
                        "type": "image_url",
                        "image_url": {
                            "url": f"data:image/jpeg;base64,{base64_image}"
                        }
                    }
                ]
            }
        ],
        "max_tokens": 1000
    }

    response = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)

    if response.status_code != 200:
        print(f"Error: API request failed with status code {response.status_code}")
        print(response.text)
        return None

    response_data = response.json()

    if 'choices' not in response_data:
        print(f"Error: Unexpected API response format: {response_data}")
        return None

    suggested_name = response_data['choices'][0]['message']['content']
    return suggested_name

# Function to sanitize file name
def sanitize_filename(suggested_name):
    name_without_extension, _ = os.path.splitext(suggested_name)
    return name_without_extension

# Function to rename images in a directory
def rename_images(directory):
    for filename in os.listdir(directory):
        if filename.lower().endswith(('.png', '.jpg', '.jpeg', '.gif', '.bmp')):
            image_path = os.path.join(directory, filename)
            base64_image = encode_image(image_path)
            suggested_name = get_image_name(base64_image)
            if suggested_name:
                sanitized_name = sanitize_filename(suggested_name).strip()
            else:
                sanitized_name = sanitize_filename(f"gpt_renaming_error_{filename}").strip()
            name_without_extension, extension = os.path.splitext(filename)
            new_filename = f"{sanitized_name}{extension}"
            new_image_path = os.path.join(directory, new_filename)
            os.rename(image_path, new_image_path)
            print(f"Renamed {filename} to {new_filename}")

# Main function to handle single image or directory
def main():
    input_path = sys.argv[1]

    if os.path.isfile(input_path):
        directory, filename = os.path.split(input_path)
        base64_image = encode_image(input_path)
        suggested_name = get_image_name(base64_image)
        if suggested_name:
            sanitized_name = sanitize_filename(suggested_name).strip()
        else:
            sanitized_name = sanitize_filename(f"gpt_renaming_error_{filename}").strip()
        name_without_extension, extension = os.path.splitext(filename)
        new_filename = f"{sanitized_name}{extension}"
        new_image_path = os.path.join(directory, new_filename)
        os.rename(input_path, new_image_path)
        print(f"Renamed {filename} to {new_filename}")
    elif os.path.isdir(input_path):
        rename_images(input_path)
    else:
        print("Invalid input path. Please provide a valid file or directory path.")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python rename_images.py <path_to_image_or_directory>")
        sys.exit(1)
    main()
