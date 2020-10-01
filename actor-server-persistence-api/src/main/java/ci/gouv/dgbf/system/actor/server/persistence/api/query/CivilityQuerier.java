package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Civility;

@Queries(value = {
		@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Civility.class,name = CivilityQuerier.QUERY_NAME_READ,value = "SELECT t FROM Civility t ORDER BY t.name ASC")
})
public interface CivilityQuerier extends Querier {

	/* read order by code ascending */
	String QUERY_NAME_READ = "readOrderByNameAscending";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(Civility.class, QUERY_NAME_READ);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements CivilityQuerier,Serializable {
		

	}
	
	/**/
	
	static CivilityQuerier getInstance() {
		return Helper.getInstance(CivilityQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		
	}
}