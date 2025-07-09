Gilded Rose Refactoring — Clean Code Edition (Python)
Introduction
This project is a clean-code refactor of the well-known Gilded Rose Kata. The goal of this refactoring effort was to apply python best practices, clean code, reduce cyclomatic complexity, and improve the maintainability and testability of the code. Key software design concepts such as the Open/Closed Principle (OCP), encapsulation, polymorphism, and modular architecture were used to restructure the core logic.
1. Refactored `update_quality` Using the Open/Closed Principle
Originally, the `update_quality` method in `GildedRose` contained a complex set of `if-elif` conditionals handling all item behaviors. This violated the Open/Closed Principle (OCP) by requiring changes to the method every time a new item type was added. We replaced this with a polymorphic structure, using item wrapper classes to handle item-specific behavior.
Old style:
if item.name == "Aged Brie":
    ...
New style:
wrapper = item_factory(item)
wrapper.update_quality()
2. Introduced a Factory Function: `item_factory()`
The factory function is responsible for returning an instance of the appropriate wrapper class based on the item name. This makes the core logic in `GildedRose` agnostic to item-specific rules and makes the system extensible without touching legacy code.
3. Created Dedicated Classes per Item Type
Each item behavior is now encapsulated in a dedicated class that extends a base wrapper. The classes include:
• AgedBrieItem
• BackstagePassItem
• SulfurasItem
• ConjuredItem
• DefaultItem
This use of polymorphism keeps the rules modular, isolated, and testable.
4. Maintained Interface Compatibility
The original `Item` class and `items` property were left untouched as required. This ensured backward compatibility with existing code and met the constraints defined in the kata.
5. Restructured Codebase for Clarity
Project structure:
python/
├── gilded_rose.py
├── item_wrappers.py
├── aged_brie.py
├── backstage.py
├── sulfuras.py
├── conjured.py
└── tests/
6. GitHub Actions CI/CD Pipeline
A GitHub Actions workflow was created to enforce code quality and automate tests. The workflow runs on pushes or PRs to the `main` and `test/my_changes` branches.
Tasks include:
• Code formatting (`black`)
• Import sorting (`isort`)
• Linting (`flake8`)
• Static code analysis (`pylint`)
• Type checking (`mypy`)
• Unit + Approval tests (`pytest`, `approvaltests`)
7. Tests Refactored
Tests were restructured into logical classes for each item type, improving readability and focus. Approval testing was preserved for regression validation.
