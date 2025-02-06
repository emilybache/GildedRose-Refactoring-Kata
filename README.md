TODO: Link to Emily's original repo and video on the refactoring process

# Getting setup

Firstly, setup a python 3.12 environment.
TODO link to venv

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
