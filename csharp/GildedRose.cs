using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.WindowsRuntime;

namespace csharp
{
    internal class GildedRose
    {
        private List<IItem> Items;
        public GildedRose(List<IItem> items)
        {
            this.Items = items;
        }

        public void UpdateQuality()
        {
            foreach (Item item in Items)
            {
                item.PassingDay();
            }
        }
    }
}
