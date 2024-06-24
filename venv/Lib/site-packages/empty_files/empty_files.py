from pathlib import Path


def create_empty_file(file_path: str) -> None:
    path = Path(file_path)
    path.parent.mkdir(parents=True, exist_ok=True)
    extension = path.suffix

    try:
        download_file(f"https://github.com/VerifyTests/EmptyFiles/raw/main/index/empty{extension}", file_path)
    except:
        open(file_path, "w").close()


def download_file(url: str, file_path: str) -> str:
    import requests as requests # this is to catch the exception if your OpenSSL environment is not setup correctly
    with requests.get(url, stream=True) as response:
        response.raise_for_status()
        with open(file_path, 'wb') as file:
            for chunk in response.iter_content(chunk_size=8192):
                file.write(chunk)
    return file_path