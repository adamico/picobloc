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

--- Create a new buffer
--- @param type string
--- @param len number
--- @return Userdata
local function new_buffer(type, len)
  if type ~= "value" then
    return userdata(type, len)
  else
    local buffer = {}
    -- buffer._len = len
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

--- Decode query parameters
--- @param param string
--- @return boolean, boolean, string
local function decode_query_param(param)
  if param:sub(1, 1) == "!" then
    assert(param:sub(-1) ~= "?", "invalid query parameter")
    return true, false, param:sub(2)
  elseif param:sub(-1) == "?" then
    return false, true, param:sub(1, -2)
  else
    return false, false, param
  end
end

--- Process query
--- @param component_list table
--- @return table
--- @return table
--- @return table
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
--- @field field_buffers table
--- @field _field_types table
--- @field _count number
--- @field _capacity number
local ComponentBuffer = {}
ComponentBuffer.__index = ComponentBuffer

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

function ComponentBuffer:add(field_values)
  if self._count == self._capacity then
    self:_grow()
  end
  local i = self._count
  self._count = self._count + 1
  for name in pairs(self._field_types) do
    self.field_buffers[name][i] = field_values[name] or 0
  end
end

function ComponentBuffer:remove(index)
  assert(0 <= index and index < self._count)
  for name in pairs(self._field_types) do
    self.field_buffers[name][index] = self.field_buffers[name][self._count - 1]
  end
  self._count = self._count - 1
end

function ComponentBuffer:get_item_fields(index)
  assert(0 <= index and index < self._count)
  local field_values = {}
  for name in pairs(self._field_types) do
    field_values[name] = self.field_buffers[name][index]
  end
  return field_values
end

---- archetype -----------------------------------

--- @class Archetype
--- @field _buffers table
--- @field _id_to_index table
--- @field _ids table
local Archetype = {}
Archetype.__index = Archetype

function Archetype.new(components_map)
  -- components_map is map of component name -> component
  local self = setmetatable({}, Archetype)
  self._buffers = {}
  self._id_to_index = {}
  self._ids = {count = 0, first = 0, last = -1} -- zero-based list
  for name, component in pairs(components_map) do
    self._buffers[name] = ComponentBuffer.new(component)
  end
  return self
end

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

function Archetype:query(components_list, fn)
  local args = {}
  for c, component in ipairs(components_list) do
    local component_buffer = self._buffers[component]
    args[c] = component_buffer and component_buffer.field_buffers
  end
  if self._ids.count > 0 then
    fn(self._ids, unpack(args, 1, #components_list))
  end
end

function Archetype:query_entity(id, components_list, fn)
  local index = assert(self._id_to_index[id], "missing entity")
  local args = {}
  for c, component in ipairs(components_list) do
    local component_buffer = self._buffers[component]
    args[c] = component_buffer and component_buffer.field_buffers
  end
  fn(index, unpack(args, 1, #components_list))
end

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

function Archetype:add_entity(id, component_values)
  -- component_values is a map of component -> table of field values
  self._id_to_index[id] = self._ids.count
  self._ids[self._ids.count] = id
  for component, buffer in pairs(self._buffers) do
    assert(type(component_values[component]) == "table",
      "component values should be tables of fields")
    buffer:add(component_values[component] or {})
  end
  self._ids.count = self._ids.count + 1
  self._ids.last = self._ids.count - 1
end

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

function Archetype:get_entity_component_values(id)
  -- returns map of component name -> {map of fieldname -> value}
  local values = {}
  local index = self._id_to_index[id]
  for component, buffer in pairs(self._buffers) do
    values[component] = buffer:get_item_fields(index)
  end
  return values
end

--- @class World
--- @field resources table
--- @_archetypes table
--- @_id_to_archetype table
--- @_next_id number
--- @_query_depth number
--- @_deferred_operations table
--- @_component_types table
local World = {}
World.__index = World


--- not used by picobloc itself, the world contains a `resources` table which
--- you can use for storing any singletons or global state that needs to be
--- accessed by systems.
function World.new()
  local self = setmetatable({}, World)
  self.resources = {}        -- for user
  self._archetypes = {}      -- list
  self._id_to_archetype = {} -- id -> archetype, false if an entity is queued for addition
  self._next_id = 1
  self._query_depth = 0
  self._deferred_operations = {}
  self._component_types = {}
  return self
end

--- Component
---
--- creates archetypes new component type. valid field types are the picotron userdata types,
---
---  or the string `'value'`, which means the field is stored in a plain
---
--- lua table instead of a userdata.
--- @param name string
--- @param fields table
function World:component(name, fields)
  assert(not self._component_types[name])
  local component = {}
  for field_name, type in pairs(fields) do
    component[field_name] = type
  end
  self._component_types[name] = component
end

--- ```lua
--- local id = world:add_entity ({ component_name = { component_field = value, ... }, ... })
--- ```
---
--- adds an entity with the given components, initializing their fields to the
--- given values. missing fields are initialized to 0. if done within a query,
--- this operation will be deferred until the query ends, so don't modify the
--- passed table after calling this.
function World:add_entity(component_values)
  assert(component_values)
  local id = self._next_id
  self._id_to_archetype[id] = false -- mark as pending
  self._next_id = self._next_id + 1
  table.insert(self._deferred_operations, function()
    self:_raw_add_entity(id, component_values)
  end)
  self:_process_deferred()
  return id
end

--- ```lua
--- world:remove_entity (id)
--- ```
---
--- removes an entity by id. if done within a query, this operation will be
--- deferred until the query ends.
function World:remove_entity(id)
  assert(self:entity_exists_or_pending(id), "tried to remove non-existent entity")
  table.insert(self._deferred_operations, function()
    self:_raw_remove_entity(id)
  end)
  self:_process_deferred()
end

--- ```lua
--- world:entity_exists (id)
--- ```
---
--- returns true if the entity exists, otherwise false. for deferred added
--- entities this will return false until they are actually added.
function World:entity_exists(id)
  return not not self._id_to_archetype[id]
end

--
--- ```lua
--- world:entity_exists_or_pending (id)
--- ```
---
--- returns true if the entity exists or has been queued for addition,
--- otherwise false
function World:entity_exists_or_pending(id)
  return self._id_to_archetype[id] ~= nil
end

--- ```lua
--- world:add_components (id, { component_name = { component_field = value, ...}, ...})
--- ```
---
--- adds components to an existing entity. field values are initialized to the
--- provided values or to 0. adding a component that is already on the entity
--- does nothing (i.e. the component values are not changed). if done within a
--- query, this operation will be deferred until the query ends, so don't
--- modify the passed table after calling this.
function World:add_components(id, new_component_values)
  assert(self:entity_exists_or_pending(id), "tried to add components to non-existent entity")
  assert(new_component_values)

  table.insert(self._deferred_operations, function()
    self:_raw_add_components(id, new_component_values)
  end)
  self:_process_deferred()
end

--- ```lua
--- world:remove_components (id, { 'component_name', ...})
--- ```
---
--- removes the named components from the entity. if done within a query, this
--- operation will be deferred until the query ends, so don't modify the passed
--- table after calling this.
function World:remove_components(id, component_list)
  assert(#component_list > 0)
  assert(self:entity_exists_or_pending(id), "tried to remove components from non-existent entity")
  table.insert(self._deferred_operations, function()
    self:_raw_remove_components(id, component_list)
  end)
  self:_process_deferred()
end

--- ```lua
--- world:query ({'component_query', ...}, function (ids, component_name, ...) ... end)
--- ```
---
--- queries all entity archetypes and calls a function for each group that
--- matches. this is the main way to access entities. `fn` is called with the
--- following arguments:
---
--- - the map of `{index -> entity id}` for all the entities in this archetype.
--- - the maps of `{field -> buffer}` for the fields of each requested component.
---   the buffers will usually be picotron userdata, but can be lua tables
---   if the corresponding field type is `'value'` (or if not running in picotron).
---
--- note that all of these buffers (userdata or table) are *zero-based*, unlike
--- typical lua. `ids.count` gives the number of entities in this batch, so to
--- loop over all the entities, use `for i = 0, ids.count-1 do ... end`.
---
--- `'component_query'` can be:
---
--- - the name of a component, which will be required, its field buffers given
---   as an argument to `fn`.
--- - a component name followed by `?`, which signals that the component is
---   optional. the corresponding argument to `fn` will be `nil` if it isn't present.
--- - `!` followed by the name of the component, which means the archetype must
---   not have the given component. no matching argument will be given to `fn`.
---
--- you may remove/add entities and components during a query, using the entity
--- ids in `ids`, but it won't actually happen until the whole query is done.
function World:query(component_list, fn)
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

--- ```lua
--- world:query_entity (id, {'component_query', ...}, function (index, component_name, ...) ... end)
--- ```
---
--- queries an individual entity. use this to access/change an individual
--- entity's values. `fn` will be given the entity's index within the provided
--- buffers. if the entity does not match the given query, `fn` will not be called.
---
--- you may remove/add entities and components during a query, but it won't
--- actually happen until the whole query is done.
function World:query_entity(id, component_list, fn)
  self._query_depth = self._query_depth + 1
  local required_components, negative_components, queried_components = process_query(component_list)
  assert(self:entity_exists(id), "entity doesn\'t exist")
  local archetype = self._id_to_archetype[id]
  if archetype:satisfies_query(required_components, negative_components) then
    archetype:query_entity(id, queried_components, fn)
  end
  self._query_depth = self._query_depth - 1
  self:_process_deferred()
end

--- ```lua
--- world:get_entity_component_values (id)
--- ```
--- creates and returns a table containing a map of
--- `{component_name -> {field_name -> field_value}}`.
--- this is a copy of the original data, so modifying it has no effect on the
--- entity. use this when you want to get all the component values, without
--- knowing in advance which components are present.
function World:get_entity_component_values(id)
  assert(self:entity_exists(id))
  return self._id_to_archetype[id]:get_entity_component_values(id)
end

function World:_find_archetype(component_set)
  -- component_set keys are the components
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

function World:_process_deferred()
  if self._query_depth == 0 and #self._deferred_operations > 0 then
    for _, op in ipairs(self._deferred_operations) do
      op()
    end
    self._deferred_operations = {}
  end
end

function World:_raw_add_entity(id, component_values)
  -- component_values is a map of component_name -> table of field values
  local a = self:_find_archetype(component_values)
  a:add_entity(id, component_values)
  self._id_to_archetype[id] = a
end

function World:_raw_remove_entity(id)
  local a = self._id_to_archetype[id]
  assert(a)
  a:remove_entity(id)
  self._id_to_archetype[id] = nil
end

function World:_raw_add_components(id, new_component_values)
  if not self:entity_exists_or_pending(id) then
    return
  end
  assert(self:entity_exists(id))
  local current_archetype = self._id_to_archetype[id]

  -- build new component set
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

function World:_raw_remove_components(id, component_list)
  if not self:entity_exists_or_pending(id) then
    return
  end
  assert(self:entity_exists(id))
  local current_archetype = self._id_to_archetype[id]

  -- build new component set
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

  -- transfer entity to the new archetype
  local component_values = current_archetype:get_entity_component_values(id)
  current_archetype:remove_entity(id)
  new_archetype:add_entity(id, component_values)
  self._id_to_archetype[id] = new_archetype
end

World._internals = {
  decode_query_param = decode_query_param,
  ComponentBuffer = ComponentBuffer,
  Archetype = Archetype
}

return World
