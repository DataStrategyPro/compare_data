SELECT
    DB_NAME() AS database_name,
    s.name AS schema_name,
    t.name AS table_name,
    c.name AS column_name,
    y.name AS data_type,
    c.max_length,
    c.precision,
    c.scale,
    CASE WHEN c.is_nullable = 0 THEN 'NOT NULL' ELSE '' END AS is_nullable,
    COALESCE(
        STUFF((
            SELECT
                ', ' + 
                CASE WHEN pk.name IS NOT NULL THEN 'PRIMARY KEY'
                     WHEN fk.name IS NOT NULL THEN 'FOREIGN KEY'
                     WHEN df.name IS NOT NULL THEN 'DEFAULT'
                     WHEN chk.name IS NOT NULL THEN 'CHECK'
                     ELSE ''
                END
            FROM sys.index_columns ic
            LEFT JOIN sys.key_constraints pk ON ic.object_id = pk.parent_object_id AND ic.index_id = pk.unique_index_id AND pk.type = 'PK'
            LEFT JOIN sys.foreign_key_columns fkc ON ic.object_id = fkc.parent_object_id AND ic.column_id = fkc.parent_column_id
            LEFT JOIN sys.objects fk ON fkc.constraint_object_id = fk.object_id AND fk.type = 'F'
            LEFT JOIN sys.default_constraints df ON c.default_object_id = df.object_id
            LEFT JOIN sys.check_constraints chk ON c.object_id = chk.parent_object_id AND c.column_id = chk.parent_column_id
            WHERE ic.object_id = t.object_id AND ic.column_id = c.column_id
            FOR XML PATH ('')), 1, 2, ''), '') AS constraint_type,
    @@SERVERNAME AS server_name
FROM sys.tables t
JOIN sys.columns c ON t.object_id = c.object_id
JOIN sys.schemas s ON t.schema_id = s.schema_id
JOIN sys.types y ON c.system_type_id = y.system_type_id AND c.user_type_id = y.user_type_id
ORDER BY DB_NAME(), s.name, t.name, c.column_id;