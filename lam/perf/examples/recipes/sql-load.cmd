💻 $ lam csv2sqlite.lam expenses.csv money.db

💻 $ sqlite3 money.db 'SELECT count(*), sum(amount) FROM expenses'
