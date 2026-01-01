local World = require("lib/picobloc/picobloc")
local internals = World._internals or {}

local decode_query_param = internals.decode_query_param
local ComponentBuffer = internals.ComponentBuffer
local Archetype = internals.Archetype

local function test_decode_query_param()
   if not decode_query_param then return end -- Skip if internal not exposed

   local neg, opt, name
   neg, opt, name = decode_query_param("!negative_component")
   assert(neg)
   assert(not opt)
   assert(name == "negative_component")
   neg, opt, name = decode_query_param("optional_component?")
   assert(not neg)
   assert(opt)
   assert(name == "optional_component")
   neg, opt, name = decode_query_param("required_component")
   assert(not neg)
   assert(not opt)
   assert(name == "required_component")
   local success = pcall(function()
      decode_query_param("!error?")
   end)
   assert(not success, "should throw an error on invalid query")
   print("test_decode_query_param passed")
end

local function test_component_buffer()
   if not ComponentBuffer then return end -- Skip if internal not exposed

   local buffer = ComponentBuffer.new {x = "f64", y = "f64"}
   assert(buffer._count == 0, "initial count should be 0")

   -- test adding elements
   buffer:add {x = 1, y = 2}
   buffer:add {x = 3, y = 4}
   buffer:add {x = 5, y = 6}
   assert(buffer._count == 3, "count should be 3 after adding three elements")

   -- test buffer growth
   for i = 4, 10 do buffer:add {x = i, y = i * 2} end
   assert(buffer._capacity >= 10, "capacity should be >=10")
   assert(buffer._count == 10, "count should be 10")

   -- check that the first item is still there after growing the buffer
   local values = buffer:get_item_fields(0)
   assert(values.x == 1 and values.y == 2, "original values should still be present after growing the buffer")

   -- test removing an element (remove the second element, index 1)
   buffer:remove(1) -- swaps with the last element before removing
   assert(buffer._count == 9, "count should be 9")

   -- ensure that the element at index 1 is now what was the last element
   values = buffer:get_item_fields(1)
   assert(values.x == 10 and values.y == 20, "values at index 1 should now be the last element's values")

   -- verify that the last element was removed by checking values at the new last index
   local last_values = buffer:get_item_fields(8)
   assert(last_values.x == 9 and last_values.y == 18,
      "values at the last index should match the previous second-to-last index's values")

   print("ComponentBuffer tests passed")
end

local function test_archetype()
   if not Archetype then return end -- Skip if internal not exposed

   local archetype = Archetype.new {position = {x = "f64", y = "f64"}}

   -- test adding entities
   archetype:add_entity(1, {position = {x = 1, y = 2}})
   archetype:add_entity(2, {position = {x = 3, y = 4}})
   assert(archetype._ids.count == 2, "count should be 2 after adding entities")

   -- test matching component set
   local matches = archetype:matches_component_set_exactly {position = true}
   assert(matches, "should exactly match the component set")

   -- test removing an entity
   archetype:remove_entity(1)
   assert(archetype._ids.count == 1, "count should be 1 after removing an entity")
   print("Archetype tests passed")
end

local function test_world()
   local world = World.new()
   world:component("position", {
      x = "f64",
      y = "f64",
   })
   world:component("size", {
      value = "f64",
   })

   -- test adding entities
   local id = world:add_entity {position = {x = 5, y = 10}}
   assert(world._next_id == 2, "next id should be incremented")
   assert(world._id_to_archetype[id], "entity should have an archetype")

   -- test removing entities
   world:remove_entity(id)
   assert(not world._id_to_archetype[id], "entity should no longer be tracked after removal")

   -- test adding components
   id = world:add_entity {position = {x = 5, y = 10}}
   world:add_components(id, {size = {value = 15}})
   assert(world._id_to_archetype[id]._buffers.size, "new component size should be added to the entity")

   -- test query
   local call_count = 0
   local new_id
   world:query_entity(id, {"position", "size"}, function(index, position, size)
      call_count = call_count + 1
      assert(position.x[index] == 5)
      assert(position.y[index] == 10)
      assert(size.value[index] == 15)
      new_id = world:add_entity {position = {}}
      assert(world:entity_exists(new_id) == false, "entity shouldn't exist yet")
      assert(world:entity_exists_or_pending(new_id) == true, "entity should be pending")
   end)
   assert(world:entity_exists(new_id) == true, "entity should exist after query")
   assert(call_count == 1, "query fn should be called once")
   call_count = 0
   world:query_entity(id, {"nonexistent_component"}, function(_index, _)
      call_count = call_count + 1
   end)
   assert(call_count == 0, "query fn should not be called on nonexistent component")
   call_count = 0
   world:query({"position", "size"}, function(ids, position, size)
      for i = 0, ids.count - 1 do
         call_count = call_count + 1
         assert(ids.count == 1)
         assert(ids[i] == id)
         assert(position.x[i] == 5)
         assert(position.y[i] == 10)
         assert(size.value[i] == 15)
      end
   end)
   assert(call_count == 1, "query fn should be called on one entity")

   -- test getting component values
   local tbl = world:get_entity_component_values(id)
   assert(tbl.position.x == 5)
   assert(tbl.position.y == 10)
   assert(tbl.size.value == 15)

   -- test removing components
   assert(world._id_to_archetype[id]._buffers.position, "component position should be on the entity before removal")
   world:remove_components(id, {"position"})
   assert(not world._id_to_archetype[id]._buffers.position, "component position should be removed from the entity")

   -- test removal during query
   call_count = 0
   world:query({"size"}, function(ids, _size)
      for i = 0, ids.count - 1 do
         call_count = call_count + 1
         world:remove_entity(ids[i])
         assert(world._id_to_archetype[ids[i]], "entity shouldn't be removed during query")
      end
   end)
   assert(call_count == 1)
   assert(not world._id_to_archetype[id], "entity should be removed after query")

   print("World tests passed")
end

local function test_advanced_queries()
   local world = World.new()
   world:component("position", {x = "f64", y = "f64"})
   world:component("health", {value = "f64"})
   world:component("velocity", {x = "f64", y = "f64"})

   local id = world:add_entity {position = {x = 100, y = 200}}
   world:add_entity {position = {x = 150, y = 225}, health = {value = 75}}
   world:add_entity {position = {x = 200, y = 250}, velocity = {x = 10, y = 10}, health = {value = 50}}

   local count = 0
   local health_count = 0
   world:query({"!velocity", "health?", "position",}, function(ids, health, position, ...)
      assert(select("#", ...) == 0, "query function should only get three arguments")
      assert(position, "position should be provided as third argument")
      for i = 0, ids.count - 1 do
         count = count + 1
         if health then
            health_count = health_count + 1
            assert(health.value[i] == 75, "incorrect health value")
         else
            assert(health == nil)
         end
      end
   end)

   world:query_entity(id, {"!velocity", "health?", "position",}, function(_index, health, position, ...)
      assert(select("#", ...) == 0, "query function should only get three arguments")
      assert(health == nil, "health should be nil")
      assert(position, "position should be provided as third argument")
   end)

   assert(health_count == 1, "should only find one health component")
   assert(count == 2, "only entities without velocity should match, regardless of health presence")

   print("test for combined negative and optional components passed")
end

test_decode_query_param()
test_component_buffer()
test_archetype()
test_world()
test_advanced_queries()
