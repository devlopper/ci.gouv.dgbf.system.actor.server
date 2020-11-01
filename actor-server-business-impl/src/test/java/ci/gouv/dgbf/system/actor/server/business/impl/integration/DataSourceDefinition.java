package ci.gouv.dgbf.system.actor.server.business.impl.integration;

/*@javax.annotation.sql.DataSourceDefinition(
		name="java:/actor/server/dataSource",
		className="org.h2.jdbcx.JdbcDataSource",
		url="jdbc:h2:mem:test;DB_CLOSE_DELAY=-1;DB_CLOSE_ON_EXIT=FALSE",
		user="sa",
		password="sa"
)*/

@javax.annotation.sql.DataSourceDefinition(
		name="java:/actor/server/dataSource",
		className="oracle.jdbc.pool.OracleDataSource",
		url="jdbc:oracle:thin:@localhost:1521:XE",
		user="SIIBC_Actor",
		password="actor"
)
public class DataSourceDefinition {}
