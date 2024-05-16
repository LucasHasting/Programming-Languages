#!/usr/local/bin/ruby
require 'pstore'
class Student
    attr_accessor :name, :major, :gender, :age;
end
store = PStore.new("disneyFile")
people = []
store.transaction do
    people = store[:people]
end
people.each do |person|
    puts person.name
    puts person.major
    puts person.gender
    puts person.age
    puts
end
