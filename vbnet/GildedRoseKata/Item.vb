Public Class Item
    Public Property Name As String
    Public Property SellIn As Integer
    Public Property Quality As Integer

    Public Overrides Function ToString() As String
        Return Me.Name & ", " & Me.SellIn & ", " & Me.Quality
    End Function
End Class
