<?php

declare(strict_types=1);

use PhpCsFixer\Fixer\ArrayNotation\ArraySyntaxFixer;
use Symfony\Component\DependencyInjection\Loader\Configurator\ContainerConfigurator;
use Symplify\EasyCodingStandard\ValueObject\Option;
use Symplify\EasyCodingStandard\ValueObject\Set\SetList;

return static function (ContainerConfigurator $containerConfigurator): void {
    $services = $containerConfigurator->services();
    $services->set(ArraySyntaxFixer::class)
        ->call('configure', [[
            'syntax' => 'short',
        ]]);

    $parameters = $containerConfigurator->parameters();
    $parameters->set(Option::PATHS, [
        __DIR__ . '/fixtures',
        __DIR__ . '/src',
        __DIR__ . '/tests',
    ]);

    $parameters->set
    (Option::SETS,
        [
            SetList::CLEAN_CODE,
            SetList::COMMON,
            SetList::PSR_12,
        ]
    );

    $parameters->set(Option::INDENTATION, "spaces");

    $parameters->set(Option::LINE_ENDING, "\n");
};
