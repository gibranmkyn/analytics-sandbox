import psycopg2

# establishing connection to redshift database
conn = psycopg2.connect(
    host=redshift_host,
    user=redshift_user,
    port=redshift_port,
    password=redshift_pwd,
    dbname=redshift_dbname
)
cur = conn.cursor()


# sql script
sql_query = ("""
                            select 1;
                            COMMIT;
                            """)

cur.execute(sql_query)

conn.commit()
conn.close()