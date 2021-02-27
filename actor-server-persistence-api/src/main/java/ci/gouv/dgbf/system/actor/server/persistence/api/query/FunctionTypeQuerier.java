package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.server.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.FunctionType;

@Queries(value = {
		@org.cyk.utility.persistence.server.annotation.Query(tupleClass = FunctionType.class,name = FunctionTypeQuerier.QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING,value = "SELECT t FROM FunctionType t ORDER BY t.code ASC")
})
public interface FunctionTypeQuerier extends Querier {

	/* read order by code ascending */
	String QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ_ORDER_BY_CODE_ASCENDING = QueryIdentifierBuilder.getInstance().build(FunctionType.class, QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING);
	Collection<FunctionType> readOrderByCodeAscending();
		
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements FunctionTypeQuerier,Serializable {	
		@Override
		public Collection<FunctionType> readOrderByCodeAscending() {
			return EntityReader.getInstance().readMany(FunctionType.class, QUERY_IDENTIFIER_READ_ORDER_BY_CODE_ASCENDING);
		}
	}
	
	/**/
	
	static FunctionTypeQuerier getInstance() {
		return Helper.getInstance(FunctionTypeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	/**/
	
	static void initialize() {
		
	}
}