# Examples

###### Select all columns

```SQL
SELECT * FROM 'users'
```

```haskell
select everything from Users
```



###### Select some columns

```SQL
SELECT name, age FROM 'users'
```

```haskell
select (Name, Age) from Users
```



###### Select with a condition

```SQL
SELECT * FROM 'users' WHERE name = 'peter'
```

```haskell
-- using the reverse application operator & from Data.Function (recommended).
select everything from Users & where' (Name `eq` "peter")

-- using regular function application
where' (Name `eq` "peter") (select everything from Users)
```
