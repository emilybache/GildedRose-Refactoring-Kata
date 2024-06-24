from typing import Any, Callable, Optional, Collection

from approvaltests import verify
from approvaltests.core.options import Options


class Storyboard:
    def __init__(
        self,
        *,  # enforce keyword arguments - https://www.python.org/dev/peps/pep-3102/
        verify_on_exit: bool = False,
        options: Optional[Options] = None,
    ) -> None:
        self.frame_number = 0
        self.story = ""
        self.add_new_line = False
        self.verify_on_exit = verify_on_exit
        self.options = options

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.verify_on_exit:
            verify(self, options=self.options)

    def add_frame(self, data: Any, title: Optional[str] = None) -> "Storyboard":
        if self.add_new_line:
            self.story += "\n"
            self.add_new_line = False
        if title:
            self.story += f"{title}:\n"
        elif self.frame_number == 0:
            self.story += "Initial:\n"
        else:
            self.story += f"Frame #{self.frame_number}:\n"
        self.story += f"{data}\n\n"
        self.frame_number += 1

        return self

    def add_frames(
        self, number_of_frames: int, function_for_frame: Callable[[int], Any]
    ) -> "Storyboard":
        for number in range(number_of_frames):
            self.add_frame(function_for_frame(number))
        return self

    def __str__(self) -> str:
        return self.story

    def iterate_frames(
        self, data: Collection[Any], number_of_frames: int = -1
    ) -> "Storyboard":
        if number_of_frames == -1:
            try:
                number_of_frames = len(data)
            except Exception as ex:
                raise RuntimeError(
                    "You must pass in the number of frames for this iterable."
                ) from ex
        for _, frame in zip(range(number_of_frames), data):
            self.add_frame(frame)
        return self

    def add_description(self, description):
        self.story += f"{description}\n\n"
        return self

    def add_description_with_data(self, description: str, data: Any) -> "Storyboard":
        self.story += f"{description}: {data}\n"
        self.add_new_line = True
        return self


StoryBoard = Storyboard


def verify_storyboard(
    *,  # enforce keyword arguments - https://www.python.org/dev/peps/pep-3102/
    options: Optional[Options] = None,
):
    return Storyboard(verify_on_exit=True, options=options)
