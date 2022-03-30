<?php

declare(strict_types=1);

namespace GildedRose\Core;

use Exception;
use Psr\Container\ContainerInterface;
use ReflectionClass;
use ReflectionException;
use ReflectionMethod;

class Container implements ContainerInterface
{
    private $deps = [];

    /**
     * Get Dependency and Respect Dependency Inversion principle
     * The Interface/Class Binding is gotten from ioc config file
     * Where we set bindings between interfaces,custom classes,aliases.. mostly anything can be injected
     */
    public function get(string $name, array $args = null): object
    {
        if (! isset($this->deps[$name])) {
            $this->deps[$name] = $name;
        }
        try {
            $reflection = new ReflectionClass($this->deps[$name]);

            if (! $reflection->isInstantiable()) {
                if (! $reflection->isInterface()) {
                    throw new Exception("{$reflection->getName()} is not instantiable !");
                }

                $reflection = new ReflectionClass(resolve($reflection->getName()));
            }
        } catch (ReflectionException $e) {
            throw new Exception($e->getMessage());
        }
        $constructor = $reflection->getConstructor();
        return $constructor === null ?
            $reflection->newInstanceArgs() :
            $reflection->newInstanceArgs($this->resolveArgs($constructor, $args));
    }

    public function has(string $name): bool
    {
        return isset($this->deps[$name]);
    }

    private function resolveArgs(ReflectionMethod $constructor, $args)
    {
        $args = $args ?? [];

        $params = $constructor->getParameters();

        foreach ($params as $param) {
            if ($param->getType() !== null && ! $param->getType()->isBuiltin()) {
                if (! array_key_exists($param->getName(), $args)) {
                    $args[] = $this->get(
                        $param->getType()->getName()
                    );
                }
            } elseif ($param->isDefaultValueAvailable()) {
                $args[] = $param->getDefaultValue();
            }
        }
        return array_values($args);
    }
}
