using System;

namespace TestNinja.Fundamentals
{
    public class DateHelper
    {
        public static DateTime FirstOfNextMonth(DateTime date)
        {
            return date.Month == 12 ? new DateTime(date.Year + 1, 1, 1) : new DateTime(date.Year, date.Month + 1, 1);
        }
    }
}