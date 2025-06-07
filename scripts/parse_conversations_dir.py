import re
import json
from pathlib import Path
from datetime import datetime

import typer
from typing_extensions import Annotated


SPACES_PATTERN = re.compile(r"\s+")
TITLE_PATTERN = re.compile(r"\* (.*?)\s+\[(.*?)\]")
PROPERTIES_PATTERN = re.compile(r':PROPERTIES:\n(.*?):END:', re.DOTALL)
MESSAGES_PATTERN = re.compile(r"\*\* (User|Assistant)\n(.*?)(?=\*\* |\Z)", re.DOTALL)

app = typer.Typer(
    name="ellm-parser",
    help="Parse ELLM conversation files from org-mode format to JSON.",
    add_completion=False
)

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
        if key == "temperature":
            properties[key] = float(properties[key])

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


def parse_conversations_file(ellm_conversations_filepath: str):
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


def glob_conversations_files(ellm_conversations_dir: Path) -> list[str]:
    """Retrieve all the filepaths of conversations file in the directory."""
    if not ellm_conversations_dir.is_dir():
        raise ValueError(f"Directory not found: {ellm_conversations_dir}")

    return [str(p) for p in ellm_conversations_dir.glob("conversations-*.org")]


@app.command()
def parse(
    conversations_dir: Annotated[
        Path,
        typer.Argument(
            help="Directory containing ELLM conversation files (conversations-*.org)",
            exists=True,
            file_okay=False,
            dir_okay=True,
            readable=True
        )
    ],
    output_dir: Annotated[
        Path,
        typer.Argument(
            help="Directory where the output JSON file (ellm.json) will be saved",
            file_okay=False,
            dir_okay=True,
            writable=True
        )
    ],
    output_filename: Annotated[
        str,
        typer.Option(
            "--output-filename", "-o",
            help="Name of the output JSON file (without extension)"
        )
    ] = "ellm",
    print_output: Annotated[
        bool,
        typer.Option(
            "--print/--no-print", "-p/-np",
            help="Print the parsed conversations to stdout"
        )
    ] = True,
    verbose: Annotated[
        bool,
        typer.Option(
            "--verbose", "-v",
            help="Enable verbose output with processing information"
        )
    ] = False
):
    """
    Parse ELLM conversation files from org-mode format to JSON.

    This command processes all conversations-*.org files in the specified directory
    and converts them to a single JSON file containing all parsed conversations.

    Each conversation includes:
    - Title and timestamp
    - Project name (extracted from filename)
    - Properties (like temperature settings)
    - Messages with role (user/assistant) and content

    Example usage:
        ellm-parser parse ./conversations ./output
        ellm-parser parse ./conversations ./output --output-filename my_conversations --no-print
    """
    # Create output directory if it doesn't exist
    output_dir.mkdir(parents=True, exist_ok=True)

    if verbose:
        typer.echo(f"Searching for conversation files in: {conversations_dir}")

    try:
        ellm_conversations_files = glob_conversations_files(conversations_dir)
    except ValueError as e:
        typer.echo(f"Error: {e}", err=True)
        raise typer.Exit(1)

    if not ellm_conversations_files:
        typer.echo(f"No conversation files found in {conversations_dir}", err=True)
        typer.echo("Looking for files matching pattern: conversations-*.org", err=True)
        raise typer.Exit(1)

    if verbose:
        typer.echo(f"Found {len(ellm_conversations_files)} conversation files:")
        for file in ellm_conversations_files:
            typer.echo(f"  - {file}")

    conversations = []
    for file in ellm_conversations_files:
        if verbose:
            typer.echo(f"Processing: {file}")

        file_conversations = parse_conversations_file(file)
        conversations.extend(file_conversations)

        if verbose:
            typer.echo(f"  Parsed {len(file_conversations)} conversations")

    output_path = output_dir / f"{output_filename}.json"

    if verbose:
        typer.echo(f"Writing {len(conversations)} total conversations to: {output_path}")

    with open(output_path, "w") as f:
        json.dump(conversations, f, default=str, indent=2)

    typer.echo(f"Successfully parsed {len(conversations)} conversations to {output_path}")

    if print_output:
        typer.echo("\nParsed conversations:")
        typer.echo(json.dumps(conversations, indent=2, default=str))


@app.command()
def validate(
    conversations_dir: Annotated[
        Path,
        typer.Argument(
            help="Directory containing ELLM conversation files to validate",
            exists=True,
            file_okay=False,
            dir_okay=True,
            readable=True
        )
    ]
):
    """
    Validate ELLM conversation files without generating output.

    This command checks if the conversation files can be parsed successfully
    and reports any issues found.
    """
    try:
        ellm_conversations_files = glob_conversations_files(conversations_dir)
    except ValueError as e:
        typer.echo(f"Error: {e}", err=True)
        raise typer.Exit(1)

    if not ellm_conversations_files:
        typer.echo(f"No conversation files found in {conversations_dir}", err=True)
        raise typer.Exit(1)

    total_conversations = 0
    errors = []

    for file in ellm_conversations_files:
        typer.echo(f"Validating: {file}")
        try:
            file_conversations = parse_conversations_file(file)
            total_conversations += len(file_conversations)
            typer.echo(f"  ✓ {len(file_conversations)} conversations parsed successfully")
        except Exception as e:
            error_msg = f"  ✗ Error parsing {file}: {e}"
            typer.echo(error_msg, err=True)
            errors.append(error_msg)

    if errors:
        typer.echo(f"\nValidation completed with {len(errors)} errors:")
        for error in errors:
            typer.echo(error, err=True)
        raise typer.Exit(1)
    else:
        typer.echo(f"\n✓ All files validated successfully! Total conversations: {total_conversations}")


if __name__ == "__main__":
    app()
