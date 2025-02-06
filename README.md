This is a refactoring and implementation kata for the [Gilded Rose Kata](https://github.com/emilybache/GildedRose-Refactoring-Kata).
In the main branch, I've used the lift up conditional refactoring, inspired by Emily Bache and Llewellyn Falco.

See their videos here:

TODO: link vids

- Emily Bache: 
- Llewellyn Falco:

# Getting setup

Firstly, setup a python 3.12 environment using [venv](https://docs.python.org/3/library/venv.html)

Then, install the requirements:

```bash
pip install -r requirements.txt
```

# Running the tests

This project uses [characterisation tests](TODO: Link to characterisation/approval tests concept) to ensure the behavior of the code remains the same during refactoring.


```bash
coverage run --branch -m pytest --approvaltests-add-reporter=diffuse -s ;
coverage html ;
coverage report --fail-under=100
```

This will run the tests, and will also:

- automatically bring up [diffuse](TODO: link to) to view differences side by side, if there are any behavior changes
- warn you if your coverage falls below 100% (either you need more tests, or there is dead code)
- generate a coverage report (You can view this in the `htmlcov` directory by opening `index.html` in a browser)
