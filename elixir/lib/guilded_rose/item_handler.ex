defmodule ItemHandler do
  @callback handle(Item) :: Item
  @callback is_handled?(String.t()) :: boolean()
end