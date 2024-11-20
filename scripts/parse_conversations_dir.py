#!/usr/bin/env python3

import re
import sys
import json
from pathlib import Path
from datetime import datetime


SPACES_PATTERN = re.compile(r"\s+")
TITLE_PATTERN = re.compile(r"\* (.*?)\s+\[(.*?)\]")
PROPERTIES_PATTERN = re.compile(r':PROPERTIES:\n(.*?):END:', re.DOTALL)
MESSAGES_PATTERN = re.compile(r"\*\* (User|Assistant)\n(.*?)(?=\*\* |\Z)", re.DOTALL)


def parse_properties(property_drawer):
    """Parse an org property drawer from a string to a dictionary."""
    properties = {}
    if not property_drawer:
        return properties

    lines = [line.strip() for line in property_drawer.split('\n')]
    lines = [line for line in lines if line]

    for line in lines:
        key, value = SPACES_PATTERN.split(line, maxsplit=2)
        key = key[1:-1].lower().strip()
        properties[key] = value.strip()

    return properties


def parse_subtree(org_subtree):
    """Parse a conversation string into a dictionary."""
    title_match = TITLE_PATTERN.search(org_subtree)
    if not title_match:
        return None

    title = title_match.group(1).strip()

    timestamp_str = title_match.group(2).strip()
    try:
        timestamp = datetime.strptime(timestamp_str, "%Y-%m-%d %a %H:%M")
    except ValueError:
        timestamp = datetime.strptime(timestamp_str, "%Y-%m-%d %a")

    properties_match = PROPERTIES_PATTERN.search(org_subtree)
    properties = parse_properties(properties_match.group(1) if properties_match else "")

    messages = []
    message_matches = MESSAGES_PATTERN.finditer(org_subtree)

    for message in message_matches:
        role = message.group(1).lower()
        content = message.group(2).strip()

        # Remove property drawers from message subtrees
        content = PROPERTIES_PATTERN.sub("", content)

        messages.append({
            "role": role,
            "content": content.strip()
        })

    return {
        "title": title,
        "timestamp": timestamp,
        "properties": properties,
        "messages": messages
    }


def split_subtrees(org_content_lines: list[str]) -> list[str]:
    """Parse a conversations file into an array of conversation strings."""
    trees = []
    current_tree = []
    is_in_tree = False

    for line in org_content_lines:
        if line.startswith("* "):
            if is_in_tree:
                trees.append(''.join(current_tree))
                current_tree = []

            current_tree.append(line)
            is_in_tree = True

        elif is_in_tree and not line.startswith("* "):
            current_tree.append(line)

    if current_tree:
        trees.append(''.join(current_tree))

    return trees


def parse_conversations_file(ellm_conversations_filepath):
    """Parse all conversations in the file into an array of dictionaries."""
    project_name = ellm_conversations_filepath.split('/')[-1].split("-", maxsplit=1)[1].split(".")[0]

    with open(ellm_conversations_filepath) as f:
        org_content_lines = f.readlines()

    subtrees = split_subtrees(org_content_lines)

    conversations = []
    for subtree in subtrees:
        try:
            conversation = parse_subtree(subtree)
            if conversation is not None:
                conversation["project"] = project_name
                conversations.append(conversation)
        except:
            continue

    return conversations


def glob_conversations_files(ellm_conversations_dir):
    """Retrieve all the filepaths of conversations file in the directory."""
    dir_path = Path(ellm_conversations_dir)

    if not dir_path.is_dir():
        raise ValueError(f"Directory not found: {directory_path}")

    return map(str, dir_path.glob("conversations-*.org"))


if __name__ == "__main__":
    ellm_conversations_directory = sys.argv[1]

    ellm_conversations_files = glob_conversations_files(ellm_conversations_directory)

    conversations = []
    for file in ellm_conversations_files:
        conversations.extend(parse_conversations_file(file))

    print(json.dumps(conversations, indent=2, default=str))
