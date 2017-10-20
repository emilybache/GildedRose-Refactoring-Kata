using System;

namespace csharp.StrategyPatternExample
{
    /// <summary>
    /// Interface that every category strategy will have to implement.
    /// </summary>
    interface ICategoryStrategy
    {
        void Update(Item item);
    }
}
