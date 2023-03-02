<?php

declare(strict_types=1);

use PhpCsFixer\Fixer\ArrayNotation\ArraySyntaxFixer;
use PhpCsFixer\Fixer\Strict\DeclareStrictTypesFixer;
use Symplify\EasyCodingStandard\Config\ECSConfig;
use Symplify\EasyCodingStandard\ValueObject\Set\SetList;

// composer require --dev symplify/easy-coding-standard
// vendor/bin/ecs init

return static function (ECSConfig $ecsConfig): void {
    $ecsConfig->paths([
        __DIR__ . '/src',
        __DIR__ . '/tests',
        __DIR__ . '/ecs.php',  // check this file too!
    ]);

    $ecsConfig->skip([
        // rules to skip
    ]);

    // run and fix, one by one
    $ecsConfig->sets([
        SetList::SPACES,
        SetList::ARRAY,
        SetList::DOCBLOCK,
        SetList::NAMESPACES,
        SetList::CONTROL_STRUCTURES,
        SetList::CLEAN_CODE,
        SetList::STRICT,
        SetList::PSR_12,
        SetList::PHPUNIT,
    ]);

    // add declare(strict_types=1); to all php files:
    $ecsConfig->rule(DeclareStrictTypesFixer::class);

    // change $array = array(); to $array = [];
    $ecsConfig->ruleWithConfiguration(ArraySyntaxFixer::class, [
        'syntax' => 'short',
    ]);

    // [default: PHP_EOL]; other options: "\n"
    $ecsConfig->lineEnding("\n");
};
