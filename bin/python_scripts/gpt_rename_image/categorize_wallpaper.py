#!/usr/bin/env python3

import base64
import requests
import os
from dotenv import load_dotenv

# OpenAI API Key
OPENAI_API_KEY = os.environ.get("OPENAI_API_KEY_WORK",
                                "<your OpenAI API key if not set as env var>")

# Function to encode the image
def encode_image(image_path):
  with open(image_path, "rb") as image_file:
    return base64.b64encode(image_file.read()).decode('utf-8')

# Path to your image
image_path = "./246501_deus-ex_-kiborg_-robot_2560x1600_(www.GdeFon.jpeg"

# Getting the base64 string
base64_image = encode_image(image_path)

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
          "text": "You are part of a python script made for categorizing wallpaper images based on what they contain. Please provide a good general category for the image. Only provide the cateogry with no explination or extra wording. You're response will be used to literally categorize the wallpaper image automatically. It's critical that you only give a general wallpaper image category that is as general as possible while still being approprate for the wallpaper's contents."
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
  "max_tokens": 300
}

response = requests.post("https://api.openai.com/v1/chat/completions", headers=headers, json=payload)

print(response.json())
