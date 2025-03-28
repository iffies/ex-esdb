defmodule ExESDB.EventStreamReaderTest do
  use ExUnit.Case
 
  doctest ExESDB.EventStreamReader

  alias ExESDB.EventStreamReader

  describe "GIVEN a store with a stream" do
    test "WHEN get_current_version is called 
  THEN it returns the current version" do
      assert EventStreamReader.get_current_version(ExESDB.TestSupport.Store.store(), :test_stream) == 0
    end
  end
end
