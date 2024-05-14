#!/usr/bin/env python3

import re
import os


def reformat_vtt_clean(file_path):
    # Read the content of the VTT file
    with open(file_path, "r", encoding="utf-8") as file:
        content = file.read()

    # Remove the 'WEBVTT' from the first line and split the content into lines
    lines = re.sub(r"WEBVTT\n\n", "", content).split("\n")

    reformatted_text = ""
    current_speaker = ""
    current_dialogue = ""

    for line in lines:
        # Skip lines with timestamps or sequence numbers
        if line.isdigit() or "-->" in line:
            continue

        # Check if the line is a speaker line (contains a colon)
        if ":" in line:
            speaker, dialogue = line.split(":", 1)
            speaker = speaker.strip()
            dialogue = dialogue.strip()

            if speaker == current_speaker:
                current_dialogue += " " + dialogue
            else:
                if current_speaker:
                    reformatted_text += (
                        f"{current_speaker}: {current_dialogue.strip()}\n\n"
                    )
                current_speaker = speaker
                current_dialogue = dialogue

    if current_speaker:
        reformatted_text += f"{current_speaker}: {current_dialogue.strip()}"

    return reformatted_text.strip()


def main():
    input_file_path = input("Enter the path to the VTT file: ").strip()
    if not os.path.exists(input_file_path):
        print("File not found. Please check the path and try again.")
        return

    reformatted_text = reformat_vtt_clean(input_file_path)

    output_file_path = os.path.splitext(input_file_path)[0] + "_reformatted.txt"
    with open(output_file_path, "w", encoding="utf-8") as file:
        file.write(reformatted_text)

    print(f"Reformatted file saved as: {output_file_path}")


if __name__ == "__main__":
    main()
