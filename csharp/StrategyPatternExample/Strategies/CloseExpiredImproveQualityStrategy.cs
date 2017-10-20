using System;
using System.Collections.Generic;

namespace csharp.StrategyPatternExample.Strategy
{
    internal class CloseExpiredImproveQualityStrategy : ICategoryStrategy
    {
        #region subclasses
        
        public class NextExpiredCondition
        {
            /// <summary>
            /// SellIn to check.
            /// </summary>
            public int SellInLimit { get; set; }

            /// <summary>
            /// Incremento to apply.
            /// </summary>
            public int Increment { get; set; }
        }

        #endregion

        #region Variables

        private IList<NextExpiredCondition> listConditions;

        #endregion

        #region Constructor

        public CloseExpiredImproveQualityStrategy(IList<NextExpiredCondition> conditions)
        {
            if (conditions == null && conditions.Count == 0)
            {                
                // INFO : A good candidate to Globalization.
                throw new ArgumentException("Param conditions list is empty.");
            }
            
            this.listConditions = conditions;
        }

        #endregion

        #region Methods
        
        public void Update(Item item)
        {
            item.SellIn--;

            if (item.SellIn < 0)
            {
                item.Quality = Global.MINIMUM_QUALITY;
            }
            else
            {
                int inc = 1;

                foreach (NextExpiredCondition condition in listConditions)
                {
                    if (item.SellIn < condition.SellInLimit)
                    {
                        inc = condition.Increment;
                        break;
                    }
                }

                item.Quality += inc;

                if (item.Quality > Global.MAXIMUM_QUALITY)
                {
                    item.Quality = Global.MAXIMUM_QUALITY;
                }
            }
        }
        
        #endregion
    }
}
