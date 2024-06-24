def to_string(exception: Exception) -> str:
    return f"{type(exception).__name__}: {str(exception)}"
