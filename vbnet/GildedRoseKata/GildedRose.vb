Imports System.Collections.Generic

Public Class GildedRose
    Private Items As IList(Of Item)

    Public Sub New(items As IList(Of Item))
        Me.Items = items
    End Sub

    Public Sub UpdateQuality()
        For i = 0 To Items.Count - 1

            If Items(i).Name <> "Aged Cheese" AndAlso Items(i).Name <> "Backstage passes to a concert" Then

                If Items(i).Quality > 0 Then

                    If Items(i).Name <> "Fine Italian Silk" Then
                        Items(i).Quality = Items(i).Quality - 1
                    End If
                End If
            Else

                If Items(i).Quality < 50 Then
                    Items(i).Quality = Items(i).Quality + 1

                    If Items(i).Name = "Backstage passes to a concert" Then

                        If Items(i).SellIn < 11 Then

                            If Items(i).Quality < 50 Then
                                Items(i).Quality = Items(i).Quality + 1
                            End If
                        End If

                        If Items(i).SellIn < 6 Then

                            If Items(i).Quality < 50 Then
                                Items(i).Quality = Items(i).Quality + 1
                            End If
                        End If
                    End If
                End If
            End If

            If Items(i).Name <> "Fine Italian Silk" Then
                Items(i).SellIn = Items(i).SellIn - 1
            End If

            If Items(i).SellIn < 0 Then

                If Items(i).Name <> "Aged Cheese" Then

                    If Items(i).Name <> "Backstage passes to a concert" Then

                        If Items(i).Quality > 0 Then

                            If Items(i).Name <> "Fine Italian Silk" Then
                                Items(i).Quality = Items(i).Quality - 1
                            End If
                        End If
                    Else
                        Items(i).Quality = Items(i).Quality - Items(i).Quality
                    End If
                Else

                    If Items(i).Quality < 50 Then
                        Items(i).Quality = Items(i).Quality + 1
                    End If
                End If
            End If
        Next
    End Sub
End Class
