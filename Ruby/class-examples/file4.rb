#!/usr/local/bin/ruby
require 'pstore'
class Student
    attr_accessor :name, :major, :gender, :age;
end
mickey = Student.new
mickey.name = "Mickey Mouse"
mickey.major = "Computers"
mickey.gender = "Male"
mickey.age = 22
daisy = Student.new
daisy.name = "Daisy Duck"
daisy.major = "Math"
daisy.gender = "Female"
daisy.age = 25
store = PStore.new("disneyFile")
store.transaction do
    store[:people] ||= Array.new
    store[:people] << mickey
    store[:people] << daisy
end
