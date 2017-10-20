using System;

namespace csharp.StrategyPatternExample
{
    interface ICategoryStrategy
    {
        void Update(Item item);
    }
}
