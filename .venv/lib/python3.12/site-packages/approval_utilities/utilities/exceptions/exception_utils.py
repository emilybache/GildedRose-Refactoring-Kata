def to_string(exception: BaseException) -> str:
    return f"{type(exception).__name__}: {str(exception)}"
