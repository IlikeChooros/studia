-- set global sql_mode='NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION';
-- set session sql_mode='NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_ENGINE_SUBSTITUTION';

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
