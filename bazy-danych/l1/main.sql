1.
show databases;
use sakila;
show tables;
describe film;
2.
select title
from film
where length >= 2;
3.
select title
from film
where rating = 'PG-13'
order by length asc
limit 4;
4.
select title from film 
where length in (
	select * from (
		select DISTINCT length
		from film
		order by length asc
		limit 4
	) as _
);
and where rating='PG-13';
5.
select film.title,
	language.name
from film
	inner join film.language_id = language.language_id
where film.description like '%Drama%';
6.
select f.title
from film f
	JOIN film_category fc ON f.film_id = fc.film_id
	JOIN category c ON c.category_id = fc.category_id
	AND c.name = 'Family'
WHERE f.description LIKE '%Documentary%';
7.
select f.title
from film f
	JOIN film_category fc ON f.film_id = fc.film_id
	JOIN category c ON c.category_id = fc.category_id
WHERE c.name = 'Children'
	AND NOT f.rating = 'PG-13';
8.
SELECT rating,
	COUNT(*) film_count
from film
GROUP BY rating;
9.
SELECT f.title
FROM film f
	JOIN inventory i ON i.film_id = f.film_id
	JOIN rental r ON r.inventory_id = i.inventory_id
where r.rental_date BETWEEN '2005-05-31' AND '2005-06-30'
GROUP BY f.title
ORDER BY f.title DESC;
10.
SELECT a.first_name,
	a.last_name
FROM actor a
	JOIN film_actor fa ON fa.actor_id = a.actor_id
	JOIN film f ON f.film_id = fa.film_id
	AND f.special_features = 'Deleted Scenes'
GROUP BY a.first_name,
	a.last_name
ORDER BY a.first_name,
	a.last_name;
11.
select c.customer_id,
	c.first_name,
	c.last_name
from customer c
	join rental r on r.customer_id = c.customer_ id
	join payment p on p.customer_id = c.customer_id
	and p.rental_id = r.rental_id
where r.staff_id != p.staff_id group b y c.customer_id;
12.
SELECT t.customer_id,
	t.r_count
FROM (
		select r.customer_id,
			COUNT(*) as r_count
		FROM rental r
		GROUP BY r.customer_id
	) as t
WHERE t.r_count > (
		select count(*)
		from rental r
			JOIN customer c ON c.customer_id = r.customer_id
		WHERE c.email = 'MARY.SMITH@sakilacustomer.org'
	);
13.
SELECT a1.first_name,
	a1.last_name,
	a2.first_name,
	a2.last_name,
	COUNT(DISTINCT fa1.film_id) AS films
FROM film_actor AS fa1
	JOIN film_actor fa2 ON fa1.film_id = fa2.film_id
	AND fa1.actor_id > fa2.actor_id
	JOIN actor a1 ON a1.actor_id = fa1.actor_id
	JOIN actor a2 ON a2.actor_id = fa2.actor_id
GROUP BY fa1.actor_id,
	fa2.actor_id
HAVING films > 1
ORDER BY fa1.actor_id,
	fa2.actor_id DESC;
14.
select last_name
from actor
where not actor_id in (
		select a.actor_id
		from actor a
			join film_actor fa on fa.actor_id = a.actor_id
			join film f on f.film_id = fa.film_id
			and f.title like 'C%'
		group by a.actor_id
	);
15.
select a.actor_id
FROM actor a
	JOIN film_actor fa ON fa.actor_id = a.actor_id
	JOIN film_category fc ON fc.film_id = fa.film_id
	JOIN category c ON c.category_id = fc.category_id
GROUP BY a.actor_id
HAVING COUNT(
		CASE
			WHEN c.name = 'Action' THEN 1
		END
	) < COUNT(
		CASE
			WHEN c.name = 'Horror' THEN 1
		END
	)
ORDER BY a.actor_id;
16.
SELECT c.first_name,
	c.last_name,
	c.customer_id
FROM customer c
	JOIN payment p ON p.customer_id = c.customer_id
GROUP BY c.customer_id
HAVING AVG(p.amount) < (
		select AVG(amount)
		from payment
		WHERE payment_date BETWEEN '2005-07-30 00:00:00' AND '2005-07-30 23:59:59'
			AND customer_id = c.customer_id
	);
17.
UPDATE film f
SET f.language_id =(
		SELECT language_id
		FROM language
		WHERE name = 'Italian'
	)
WHERE title = 'YOUNG LANGUAGE';
18.
INSERT INTO language (name)
VALUES ('Spanish');
UPDATE film f
	JOIN film_actor fa ON fa.film_id = f.film_id
	JOIN actor a ON a.actor_id = fa.actor_id
	AND a.first_name = 'ED'
	AND a.last_name = 'CHASE'
SET f.language_id =(
		SELECT language_id
		FROM language
		WHERE name = 'Spanish'
	);
19.
ALTER TABLE language
ADD COLUMN films_no INT UNSIGNED NOT NULL DEFAULT 0;
UPDATE language l
SET l.films_no =(
		SELECT COUNT(*)
		FROM film
		WHERE film.language_id = l.language_id
	);
20.
ALTER TABLE film DROP COLUMN release_year;
