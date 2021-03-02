package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentityGroup;

@Queries(value = {
		@org.cyk.utility.persistence.annotation.Query(tupleClass = IdentityGroup.class,name = IdentityGroupQuerier.QUERY_NAME_READ,value = "SELECT t FROM IdentityGroup t ORDER BY t.name ASC")
})
public interface IdentityGroupQuerier extends Querier {

	/* read order by code ascending */
	String QUERY_NAME_READ = "readOrderByNameAscending";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(IdentityGroup.class, QUERY_NAME_READ);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements IdentityGroupQuerier,Serializable {
		

	}
	
	/**/
	
	static IdentityGroupQuerier getInstance() {
		return Helper.getInstance(IdentityGroupQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		
	}
}