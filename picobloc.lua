-- # picobloc
--
-- an archetype and userdata-based ecs library for the
-- [picotron fantasy workstation](https://www.lexaloffle.com/picotron.php).
--
-- fork by @kc00l
-- ## license
--
-- Copyright 2024 Kira Boom
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the “Software”), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
-- sell copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
-- THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
-- DEALINGS IN THE SOFTWARE.

--------------------------------------------------
-- types
--------------------------------------------------

--- @alias EntityID integer
--- @alias ComponentName string
--- @alias ComponentType table<string, string>
--- @alias ComponentValues table<string, any>|boolean
--- @alias QueryIDs { count: integer, first: integer, last: integer, [integer]: EntityID }

---- buffer --------------------------------------

local is_picotron = rawget(_G, "userdata") ~= nil

--- @generic T
--- @param type "value"|string
--- @param len integer
--- @return userdata|table<integer, any>
local function new_buffer(type, len)
  if is_picotron and type ~= "value" then
    return userdata(type, len)
  else
    local buffer = {}
    for i = 0, len - 1 do
      buffer[i] = 0
    end

    -- todo: put this back when i know the proper usage of userdata.copy
    -- function buffer:copy (target)
    --  assert (target._len >= self._len)
    --  for i = 0, self.len do
    --    target[i] = self[i]
    --  end
    -- end

    return buffer
  end
end

---- query decoding ------------------------------

--- @param param string
--- @return boolean is_negative
--- @return boolean is_optional
--- @return string name
local function decode_query_param(param)
  if string.sub(param, 1, 1) == "!" then
    assert(string.sub(param, -1) ~= "?", "invalid query parameter")
    return true, false, string.sub(param, 2)
  elseif string.sub(param, -1) == "?" then
    return false, true, string.sub(param, 1, -2)
  else
    return false, false, param
  end
end

--- @param component_list string[]
--- @return string[] required_components
--- @return string[] negative_components
--- @return string[] queried_components
local function process_query(component_list)
  local required_components = {}
  local negative_components = {}
  local queried_components = {}
  for _, c in ipairs(component_list) do
    local neg, opt, name = decode_query_param(c)
    if neg then
      table.insert(negative_components, name)
    else
      if not opt then
        table.insert(required_components, name)
      end
      table.insert(queried_components, name)
    end
  end
  return required_components, negative_components, queried_components
end

--- @class ComponentBuffer
--- @field field_buffers table<string, userdata|table>
--- @field _field_types table<string, string>
--- @field _count integer
--- @field _capacity integer
local ComponentBuffer = {}
ComponentBuffer.__index = ComponentBuffer

--- @param field_types ComponentType
--- @return ComponentBuffer
function ComponentBuffer.new(field_types)
  local self = setmetatable({}, ComponentBuffer)
  self.field_buffers = {}
  self._field_types = {}
  self._count = 0
  self._capacity = 8
  for name, type in pairs(field_types) do
    assert(self.field_buffers[name] == nil)
    self._field_types[name] = type
    self.field_buffers[name] = new_buffer(type, self._capacity)
  end
  return self
end

function ComponentBuffer:_grow()
  self._capacity = self._capacity * 2
  for name, type in pairs(self._field_types) do
    local new = new_buffer(type, self._capacity)
    for i = 0, self._capacity / 2 - 1 do
      new[i] = self.field_buffers[name][i]
    end
    -- this doesn't work for some reason
    -- self.field_buffers [name]:copy (new, 0, 0, self._capacity/2)
    self.field_buffers[name] = new
  end
end

--- @param field_values table<string, any>
function ComponentBuffer:add(field_values)
  if self._count == self._capacity then
    self:_grow()
  end
  local i = self._count
  self._count = self._count + 1
  for name in pairs(self._field_types) do
    local val = field_values[name]
    if val == nil then val = 0 end
    self.field_buffers[name][i] = val
  end
end

--- @param index integer
function ComponentBuffer:remove(index)
  assert(0 <= index and index < self._count)
  for name in pairs(self._field_types) do
    self.field_buffers[name][index] = self.field_buffers[name][self._count - 1]
  end
  self._count = self._count - 1
end

--- @param index integer
--- @return table<string, any>
function ComponentBuffer:get_item_fields(index)
  assert(0 <= index and index < self._count)
  local field_values = {}
  for name in pairs(self._field_types) do
    field_values[name] = self.field_buffers[name][index]
  end
  return field_values
end

--- @class Archetype
--- @field _buffers table<string, ComponentBuffer>
--- @field _id_to_index table<EntityID, integer>
--- @field _ids { count: integer, first: integer, last: integer, [integer]: EntityID }
local Archetype = {}
Archetype.__index = Archetype

--- @param components_map table<ComponentName, ComponentType>
--- @return Archetype
function Archetype.new(components_map)
  local self = setmetatable({}, Archetype)
  self._buffers = {}
  self._id_to_index = {}
  self._ids = {count = 0, first = 0, last = -1} -- zero-based list
  for name, component in pairs(components_map) do
    self._buffers[name] = ComponentBuffer.new(component)
  end
  return self
end

--- @param required_components ComponentName[]
--- @param negative_components ComponentName[]
--- @return boolean
function Archetype:satisfies_query(required_components, negative_components)
  for _, component in ipairs(required_components) do
    if not self._buffers[component] then
      return false
    end
  end
  for _, component in ipairs(negative_components) do
    if self._buffers[component] then
      return false
    end
  end
  return true
end

--- @param components_list ComponentName[]
--- @param fn function
function Archetype:query(components_list, fn)
  local args = {}
  for c, component in ipairs(components_list) do
    local component_buffer = self._buffers[component]
    args[c] = component_buffer and component_buffer.field_buffers
  end
  if self._ids.count > 0 then
    --- @diagnostic disable-next-line: deprecated
    fn(self._ids, unpack(args, 1, #components_list))
  end
end

--- @param id EntityID
--- @param components_list ComponentName[]
--- @param fn function
function Archetype:query_entity(id, components_list, fn)
  local index = assert(self._id_to_index[id], "missing entity")
  local args = {}
  for c, component in ipairs(components_list) do
    local component_buffer = self._buffers[component]
    args[c] = component_buffer and component_buffer.field_buffers
  end
  --- @diagnostic disable-next-line: deprecated
  fn(index, unpack(args, 1, #components_list))
end

--- @param component_set table<ComponentName, boolean>
--- @return boolean
function Archetype:matches_component_set_exactly(component_set)
  for c, _ in pairs(component_set) do
    if not self._buffers[c] then
      return false
    end
  end
  for c, _ in pairs(self._buffers) do
    if not component_set[c] then
      return false
    end
  end
  return true
end

--- @param id EntityID
--- @param component_values table<ComponentName, ComponentValues>
function Archetype:add_entity(id, component_values)
  self._id_to_index[id] = self._ids.count
  self._ids[self._ids.count] = id
  for component, buffer in pairs(self._buffers) do
    assert(type(component_values[component]) == "table",
      "component values should be tables of fields")
    --- @cast component_values table<ComponentName, table>
    buffer:add(component_values[component] or {})
  end
  self._ids.count = self._ids.count + 1
  self._ids.last = self._ids.count - 1
end

--- @param id EntityID
function Archetype:remove_entity(id)
  local index = self._id_to_index[id]
  assert(index)
  for _, buffer in pairs(self._buffers) do
    buffer:remove(index)
  end
  local count = self._ids.count
  self._id_to_index[self._ids[count - 1]] = index
  self._ids[index] = self._ids[count - 1]
  self._ids[count - 1] = nil
  self._ids.count = count - 1
  self._ids.last = self._ids.count - 1
end

--- @param id EntityID
--- @return table<ComponentName, ComponentValues>
function Archetype:get_entity_component_values(id)
  local values = {}
  local index = self._id_to_index[id]
  for component, buffer in pairs(self._buffers) do
    values[component] = buffer:get_item_fields(index)
  end
  return values
end

--- @class ECSWorld
--- @field resources table<any, any> can be used for storing any singletons or global state that needs to been accessed by systems.
--- @field _archetypes Archetype[]
--- @field _id_to_archetype table<EntityID, Archetype|false>
--- @field _next_id EntityID
--- @field _query_depth integer
--- @field _deferred_operations function[]
--- @field _component_types table<ComponentName, ComponentType>
--- @field _archetype_by_components table<string, Archetype>
--- @field component fun(self: ECSWorld, name: ComponentName, fields: ComponentType)
--- @field tag fun(self: ECSWorld, ...)
--- @field add_entity fun(self: ECSWorld, component_values: table<ComponentName, ComponentValues>): EntityID
--- @field remove_entity fun(self: ECSWorld, id: EntityID)
--- @field query fun(self: ECSWorld, components: ComponentName[], fn: fun(ids: QueryIDs, ...))
--- @field query_entity fun(self: ECSWorld, id: EntityID, components: ComponentName[], fn: fun(index: integer, ...))
--- @field entity_exists fun(self: ECSWorld, id: EntityID): boolean
--- @field entity_exists_or_pending fun(self: ECSWorld, id: EntityID): boolean
--- @field add_components fun(self: ECSWorld, id: EntityID, new_component_values: table<ComponentName, ComponentValues>)
--- @field remove_components fun(self: ECSWorld, id: EntityID, component_list: ComponentName[])
--- @field get_entity_component_values fun(self: ECSWorld, id: EntityID): table<ComponentName, ComponentValues>
local ECSWorld = {}
ECSWorld.__index = ECSWorld

--- @return ECSWorld
function ECSWorld.new()
  local self = setmetatable({}, ECSWorld)
  self.resources = {}
  self._archetypes = {}
  self._id_to_archetype = {}
  self._next_id = 1
  self._query_depth = 0
  self._deferred_operations = {}
  self._component_types = {}
  return self
end

--- Creates a new component type. valid field types are the picotron userdata
---
--- types, or the string `'value'`, which means the field is stored in a plain
---
--- lua table instead of a userdata.
---
--- @param name ComponentName
--- @param fields ComponentType
function ECSWorld:component(name, fields)
  assert(not self._component_types[name], 'component already exists: "'..name..'"')
  local component = {}
  for field_name, type in pairs(fields) do
    component[field_name] = type
  end
  self._component_types[name] = component
end

--- Registers one or more tag components (components with no fields).
---
--- tags are used purely for filtering entities in queries.
---
--- in entity definitions, use `tag_name = true` or `tag_name = {}`.
---
--- @param ... ComponentName
function ECSWorld:tag(...)
  for _, name in ipairs({...}) do
    assert(not self._component_types[name], "tag already exists: "..name)
    self._component_types[name] = {}
  end
end

--- Adds an entity with the given components, initializing their fields to the
---
--- given values. missing fields are initialized to 0. if done within a query,
---
--- this operation will be deferred until the query ends, so don't modify the
---
--- passed table after calling this.
---
--- @param component_values table<ComponentName, ComponentValues>
--- @return EntityID
function ECSWorld:add_entity(component_values)
  assert(component_values, "missing component values")
  local id = self._next_id
  self._id_to_archetype[id] = false -- mark as pending
  self._next_id = self._next_id + 1
  table.insert(self._deferred_operations, function()
    self:_raw_add_entity(id, component_values)
  end)
  self:_process_deferred()
  return id
end

--- Removes an entity by id. if done within a query, this operation
---
--- will be deferred until the query ends.
---
--- @param id EntityID
function ECSWorld:remove_entity(id)
  assert(self:entity_exists_or_pending(id), "tried to remove non-existent entity")
  table.insert(self._deferred_operations, function()
    self:_raw_remove_entity(id)
  end)
  self:_process_deferred()
end

--- Returns true if the entity exists, otherwise false. for deferred added
---
--- entities this will return false until they are actually added.
---
--- @param id EntityID
--- @return boolean
function ECSWorld:entity_exists(id)
  return not not self._id_to_archetype[id]
end

--- Returns true if the entity exists or has been queued for addition, otherwise false
---
--- @param id EntityID
--- @return boolean
function ECSWorld:entity_exists_or_pending(id)
  return self._id_to_archetype[id] ~= nil
end

--- Adds components to an existing entity. field values are initialized to the
---
--- provided values or to 0. adding a component that is already on the entity
---
--- does nothing (i.e. the component values are not changed). if done within a
---
--- query, this operation will be deferred until the query ends, so don't
---
--- modify the passed table after calling this.
---
--- @param id EntityID
--- @param new_component_values table<ComponentName, ComponentValues>
function ECSWorld:add_components(id, new_component_values)
  assert(self:entity_exists_or_pending(id), "tried to add components to non-existent entity")
  assert(new_component_values, "missing component values")

  table.insert(self._deferred_operations, function()
    self:_raw_add_components(id, new_component_values)
  end)
  self:_process_deferred()
end

--- Removes the named components from the entity. if done within a query, this
---
--- operation will be deferred until the query ends, so don't modify the passed
---
--- table after calling this
--- @param id EntityID
--- @param component_list ComponentName[]
function ECSWorld:remove_components(id, component_list)
  assert(#component_list > 0, "component list should be > 0")
  assert(self:entity_exists_or_pending(id), "tried to remove components from non-existent entity")
  table.insert(self._deferred_operations, function()
    self:_raw_remove_components(id, component_list)
  end)
  self:_process_deferred()
end

--- Queries all entity archetypes and calls a function for each group that
---
--- matches. this is the main way to access entities. `fn` is called with the
---
--- following arguments:
---
--- - the map of `{index -> entity id}` for all the entities in this archetype.
---
--- - the maps of `{field -> buffer}` for the fields of each requested component.
---
---   the buffers will usually be picotron userdata, but can be lua tables
---
---   if the corresponding field type is `'value'` (or if not running in picotron).
---
--- note that all of these buffers (userdata or table) are *zero-based*, unlike
---
--- typical lua. `ids.count` gives the number of entities in this batch, so to
---
--- loop over all the entities, use `for i = 0, ids.count-1 do ... end`.
---
--- `'component_query'` can be:
---
--- - the name of a component, which will be required, its field buffers given
---
---   as an argument to `fn`.
---
--- - a component name followed by `?`, which signals that the component is
---
---   optional. the corresponding argument to `fn` will be `nil` if it isn't present.
---
--- - `!` followed by the name of the component, which means the archetype must
---
---   not have the given component. no matching argument will be given to `fn`.
---
--- you may remove/add entities and components during a query, using the entity
---
--- ids in `ids`, but it won't actually happen until the whole query is done.
---
--- @param component_list string[]
--- @param fn function
function ECSWorld:query(component_list, fn)
  self._query_depth = self._query_depth + 1
  local required_components, negative_components, queried_components = process_query(component_list)
  for _, a in ipairs(self._archetypes) do
    if a:satisfies_query(required_components, negative_components) then
      a:query(queried_components, fn)
    end
  end
  self._query_depth = self._query_depth - 1
  self:_process_deferred()
end

--- Queries an individual entity. use this to access/change an individual
---
--- entity's values. `fn` will be given the entity's index within the provided
---
--- buffers. if the entity does not match the given query, `fn` will not be called.
---
--- you may remove/add entities and components during a query, but it won't
---
--- actually happen until the whole query is done.
---
--- @param id EntityID
--- @param component_list ComponentName[]
--- @param fn function
function ECSWorld:query_entity(id, component_list, fn)
  self._query_depth = self._query_depth + 1
  local required_components, negative_components, queried_components = process_query(component_list)
  assert(self:entity_exists(id), "entity doesn\'t exist")
  local archetype = self._id_to_archetype[id]
  --- @cast archetype Archetype
  if archetype:satisfies_query(required_components, negative_components) then
    archetype:query_entity(id, queried_components, fn)
  end
  self._query_depth = self._query_depth - 1
  self:_process_deferred()
end

--- Creates and returns a table containing a map of
---
--- `{component_name -> {field_name -> field_value}}`.
---
--- this is a copy of the original data, so modifying it has no effect on the
---
--- entity. use this when you want to get all the component values, without
---
--- knowing in advance which components are present.
---
--- @param id EntityID
--- @return table<ComponentName, ComponentValues>
function ECSWorld:get_entity_component_values(id)
  assert(self:entity_exists(id), "entity "..id.." doesn't exist")
  local archetype = self._id_to_archetype[id]
  --- @cast archetype Archetype
  return archetype:get_entity_component_values(id)
end

--- @param component_set table<ComponentName, any>
--- @return Archetype
function ECSWorld:_find_archetype(component_set)
  for _, a in ipairs(self._archetypes) do
    if a:matches_component_set_exactly(component_set) then
      return a
    end
  end
  local component_map = {}
  for name, _ in pairs(component_set) do
    if not self._component_types[name] then
      error('tried to add entity with unknown component "'..tostring(name)..'"')
    end
    component_map[name] = self._component_types[name]
  end
  local a = Archetype.new(component_map)
  table.insert(self._archetypes, a)
  return a
end

function ECSWorld:_process_deferred()
  if self._query_depth == 0 and #self._deferred_operations > 0 then
    for _, op in ipairs(self._deferred_operations) do
      op()
    end
    self._deferred_operations = {}
  end
end

--- @param id EntityID
--- @param component_values table<ComponentName, ComponentValues>
function ECSWorld:_raw_add_entity(id, component_values)
  -- component_values is a map of component_name -> table of field values
  -- normalize `component = true` shorthand to `component = {}`
  for name, value in pairs(component_values) do
    if value == true then
      component_values[name] = {}
    end
  end
  local a = self:_find_archetype(component_values)
  a:add_entity(id, component_values)
  self._id_to_archetype[id] = a
end

--- @param id EntityID
function ECSWorld:_raw_remove_entity(id)
  local a = self._id_to_archetype[id]
  assert(a, "archetype "..id.." doesn't exist")
  --- @cast a Archetype
  a:remove_entity(id)
  self._id_to_archetype[id] = nil
end

--- @param id EntityID
--- @param new_component_values table<ComponentName, ComponentValues>
function ECSWorld:_raw_add_components(id, new_component_values)
  if not self:entity_exists_or_pending(id) then
    return
  end
  assert(self:entity_exists(id), "entity "..id.." doesn't exist")

  -- normalize `component = true` shorthand to `component = {}`
  for name, value in pairs(new_component_values) do
    if value == true then
      new_component_values[name] = {}
    end
  end

  local current_archetype = self._id_to_archetype[id]
  --- @cast current_archetype Archetype

  local new_component_set = {}
  for component, _ in pairs(current_archetype._buffers) do
    new_component_set[component] = true
  end
  for component, _ in pairs(new_component_values) do
    new_component_set[component] = true
  end

  local new_archetype = self:_find_archetype(new_component_set)
  if current_archetype == new_archetype then
    return
  end

  -- transfer entity
  local component_values = current_archetype:get_entity_component_values(id)
  for component, data in pairs(new_component_values) do
    -- don't overwrite components
    if component_values[component] == nil then
      component_values[component] = data
    end
  end
  current_archetype:remove_entity(id)
  new_archetype:add_entity(id, component_values)
  self._id_to_archetype[id] = new_archetype
end

--- @param id EntityID
--- @param component_list ComponentName[]
function ECSWorld:_raw_remove_components(id, component_list)
  if not self:entity_exists_or_pending(id) then
    return
  end
  assert(self:entity_exists(id), "entity "..id.." doesn't exist")
  local current_archetype = self._id_to_archetype[id]
  --- @cast current_archetype Archetype

  local new_component_set = {}
  for comp, _ in pairs(current_archetype._buffers) do
    new_component_set[comp] = true
  end
  for _, comp in ipairs(component_list) do
    new_component_set[comp] = nil
  end

  local new_archetype = self:_find_archetype(new_component_set)
  if current_archetype == new_archetype then
    return
  end

  local component_values = current_archetype:get_entity_component_values(id)
  current_archetype:remove_entity(id)
  new_archetype:add_entity(id, component_values)
  self._id_to_archetype[id] = new_archetype
end

---- tests ---------------------------------------

local function test_decode_query_param()
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
end

local function test_component_buffer()
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
  assert(values.x == 10 and values.y == 20, "values at index 1 should now be the last element\'s values")

  -- verify that the last element was removed by checking values at the new last index
  local last_values = buffer:get_item_fields(8)
  assert(last_values.x == 9 and last_values.y == 18,
    "values at the last index should match the previous second-to-last index\'s values")

  -- print ('ComponentBuffer tests passed')
end

local function test_archetype()
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
  -- print ('Archetype tests passed')
end

local function test_world()
  local world = ECSWorld.new()
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
    assert(world:entity_exists(new_id) == false, "entity shouldn\'t exist yet")
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
      assert(world._id_to_archetype[ids[i]], "entity shouldn\'t be removed during query")
    end
  end)
  assert(call_count == 1)
  assert(not world._id_to_archetype[id], "entity should be removed after query")

  -- print('World tests passed')
end

local function test_advanced_queries()
  local world = ECSWorld.new()
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

  -- print("test for combined negative and optional components passed")
end

test_decode_query_param()
test_component_buffer()
test_archetype()
test_world()
test_advanced_queries()

---- return --------------------------------------

--- @class PicoblocModule
--- @field World fun(): ECSWorld

if rawget(_G, "require") then
  -- if require is available, behave like a real lua module
  --- @type PicoblocModule
  return {World = ECSWorld.new}
else
  -- otherwise we're using include, so store World in a global variable
  _G.World = ECSWorld.new
end
