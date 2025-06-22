def copy_to_clipboard(text: str) -> None:
    """
    This acts as a wrapper to defer the importing of pyperclip util actual usage
    """
    import pyperclip  # type ignore

    pyperclip.copy(text)
