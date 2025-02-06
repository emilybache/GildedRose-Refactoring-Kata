This is my implementation of the [Gilded Rose Kata](https://github.com/emilybache/GildedRose-Refactoring-Kata).

## Tidy first, then change behavior

`main`

In the main branch is only refactored from the original, with the [new requirement](GildedRoseRequirements.md) yet to implemented.
I used the lift up conditional refactoring, inspired by Emily Bache and Llewellyn Falco.

See their videos here:

- [Emily Bache introduced me to the idea](https://www.youtube.com/watch?v=OdnV8hc9L7I)
- [Llewellyn Falco shows the whole process, and makes it as lazy as possible](https://www.youtube.com/watch?v=wp6oSVDdbXQ)

`feature/implement-new-requirement`

In this branch, I have implemented the new requirement.

`archive/original-code`

This branch contains the original code, before the refactoring.

# Getting setup

Firstly, setup a python 3.12 environment using [venv](https://docs.python.org/3/library/venv.html) 

Then, install the requirements:

```bash
pip install -r requirements.txt
```

# Running the tests

This project uses [characterisation testing](https://www.youtube.com/watch?v=me-Nikc5eak) to ensure the behavior of the code remains the same during refactoring.
Specifically, it uses [pytest-approvals](https://github.com/approvals/ApprovalTests.Python)


```bash
coverage run --branch -m pytest --approvaltests-add-reporter=diffuse -s ;
coverage html ;
coverage report --fail-under=100
```

This will run the tests, and will also:

- automatically bring up [diffuse](https://diffuse.sourceforge.net/download.html) to view differences side by side, if there are any behavior changes.
    - feel free to install and use a diff reporter of your choice. See pytest docs for more info on configuring reporters: [selecting a reporter](https://github.com/approvals/ApprovalTests.Python?tab=readme-ov-file#selecting-a-reporter)
- warn you if your coverage falls below 100% (either you need more tests, or there is dead code)
- generate a coverage report (You can view this in the `htmlcov` directory by opening `index.html` in a browser)
