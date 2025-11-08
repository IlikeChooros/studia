### Lista 1
Paweł Smolnicki 283999

## 1
```sql
SHOW DATABASES;
USE sakila;
SHOW TABLES;
DESCRIBE film;
```

## 2
```sql
SELECT title
FROM film
WHERE length >= 2;
```

## 3
```sql
SELECT title
FROM film
WHERE rating = 'PG-13'
ORDER BY length ASC
LIMIT 4;
```

## 4
```sql
select title from film 
where film.length in (
	select * from (
        select DISTINCT length
        from film
        order by length asc
        limit 4
    ) AS _
)
and rating='PG-13';
```

## 5
```sql
SELECT film.title, language.name
FROM film
JOIN language ON film.language_id = language.language_id
WHERE film.description LIKE '%Drama%';
```

## 6
```sql
SELECT f.title
FROM film f
JOIN film_category fc ON f.film_id = fc.film_id
JOIN category c ON c.category_id = fc.category_id
WHERE c.name = 'Family'
AND f.description LIKE '%Documentary%';
```

## 7
```sql
SELECT f.title
FROM film f
JOIN film_category fc ON f.film_id = fc.film_id
JOIN category c ON c.category_id = fc.category_id
WHERE c.name = 'Children'
AND f.rating != 'PG-13';
```

## 8
```sql
SELECT rating, COUNT(*) AS film_count
FROM film
GROUP BY rating;
```

## 9
```sql
SELECT DISTINCT f.title
FROM film f
JOIN inventory i ON i.film_id = f.film_id
JOIN rental r ON r.inventory_id = i.inventory_id
WHERE r.rental_date BETWEEN '2005-05-31' AND '2005-06-30'
ORDER BY f.title DESC;
```

## 10
```sql
SELECT DISTINCT a.first_name, a.last_name
FROM actor a
JOIN film_actor fa ON fa.actor_id = a.actor_id
JOIN film f ON f.film_id = fa.film_id
WHERE f.special_features = 'Deleted Scenes'
ORDER BY a.first_name, a.last_name;
```

## 11
```sql
SELECT c.customer_id, c.first_name, c.last_name
FROM customer c
JOIN rental r ON r.customer_id = c.customer_id
JOIN payment p ON p.customer_id = c.customer_id
AND p.rental_id = r.rental_id
AND r.staff_id != p.staff_id
GROUP BY c.customer_id;
```

## 12
```sql
SELECT COUNT(*)
FROM (
    SELECT r.customer_id, COUNT(*) AS r_count
    FROM rental r
    GROUP BY r.customer_id
) AS t
WHERE t.r_count > (
    SELECT COUNT(*)
    FROM rental r
    JOIN customer c ON c.customer_id = r.customer_id
    WHERE c.email = 'MARY.SMITH@sakilacustomer.org'
);
```

## 13
```sql
SELECT a1.first_name, a1.last_name, a2.first_name, a2.last_name, COUNT(DISTINCT fa1.film_id) AS films
FROM film_actor AS fa1
JOIN film_actor fa2 ON fa1.film_id = fa2.film_id
AND fa1.actor_id > fa2.actor_id
JOIN actor a1 ON a1.actor_id = fa1.actor_id
JOIN actor a2 ON a2.actor_id = fa2.actor_id
GROUP BY fa1.actor_id, fa2.actor_id
HAVING films > 1
ORDER BY fa1.actor_id, fa2.actor_id DESC;
```

## 14
```sql
SELECT last_name
FROM actor
WHERE actor_id NOT IN (
    SELECT a.actor_id
    FROM actor a
    JOIN film_actor fa ON fa.actor_id = a.actor_id
    JOIN film f ON f.film_id = fa.film_id
    WHERE f.title LIKE 'C%'
    GROUP BY a.actor_id
);
```

## 15
```sql
SELECT a.first_name, a.last_name
FROM actor a
JOIN film_actor fa ON fa.actor_id = a.actor_id
JOIN film_category fc ON fc.film_id = fa.film_id
JOIN category c ON c.category_id = fc.category_id
GROUP BY a.actor_id
HAVING COUNT(CASE WHEN c.name = 'Action' THEN 1 END) < COUNT(CASE WHEN c.name = 'Horror' THEN 1 END)
ORDER BY a.actor_id;
```

## 16
```sql
SELECT c.first_name, c.last_name, c.customer_id
FROM customer c
JOIN payment p ON p.customer_id = c.customer_id
GROUP BY c.customer_id
HAVING AVG(p.amount) < (
    SELECT AVG(amount)
    FROM payment
    WHERE payment_date BETWEEN '2005-07-30 00:00:00' AND '2005-07-30 23:59:59'
    AND customer_id = c.customer_id
);
```

## 17
```sql
UPDATE film f
SET f.language_id = (
    SELECT language_id
    FROM language
    WHERE name = 'Italian'
)
WHERE title = 'YOUNG LANGUAGE';
```

## 18
```sql
INSERT INTO language (name)
VALUES ('Spanish');

UPDATE film f
JOIN film_actor fa ON fa.film_id = f.film_id
JOIN actor a ON a.actor_id = fa.actor_id
SET f.language_id = (
    SELECT language_id
    FROM language
    WHERE name = 'Spanish'
)
WHERE a.first_name = 'ED'
AND a.last_name = 'CHASE';
```

## 19
```sql
ALTER TABLE language
ADD COLUMN films_no INT UNSIGNED NOT NULL DEFAULT 0;

UPDATE language l
SET l.films_no = (
    SELECT COUNT(*)
    FROM film
    WHERE film.language_id = l.language_id
);
```

## 20
```sql
ALTER TABLE film
DROP COLUMN release_year;
```