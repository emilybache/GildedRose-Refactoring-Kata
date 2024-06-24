def copy_to_clipboard(text):
    """
    This acts as a wrapper to defer the importing of pyperclip util actual usage
    """
    import pyperclip  # type ignore

    pyperclip.copy(text)
