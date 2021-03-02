package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryHelper;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryIdentifierGetter;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

@Queries(value = {
		@org.cyk.utility.persistence.annotation.Query(tupleClass = ScopeType.class,name = ScopeTypeQuerier.QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING
				,value = "SELECT t FROM ScopeType t ORDER BY t.code ASC")
		,@org.cyk.utility.persistence.annotation.Query(tupleClass = ScopeType.class,name = ScopeTypeQuerier.QUERY_NAME_READ_ORDER_BY_ORDER_NUMBER
			,value = "SELECT t FROM ScopeType t ORDER BY t.orderNumber ASC,t.code ASC")
})
public interface ScopeTypeQuerier extends Querier.CodableAndNamable<ScopeType> {

	/* read order by code ascending */
	String QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ_ORDER_BY_CODE_ASCENDING = QueryIdentifierBuilder.getInstance().build(ScopeType.class, QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING);
	Collection<ScopeType> readOrderByCodeAscending();
	
	String QUERY_NAME_READ_ORDER_BY_ORDER_NUMBER = "readOrderByOrderNumberAscending";
	String QUERY_IDENTIFIER_READ_ORDER_BY_ORDER_NUMBER_ASCENDING = QueryIdentifierBuilder.getInstance().build(ScopeType.class, QUERY_NAME_READ_ORDER_BY_ORDER_NUMBER);
	Collection<ScopeType> readOrderByOrderNumberAscending();
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<ScopeType> implements ScopeTypeQuerier,Serializable {	
		@Override
		public Collection<ScopeType> readOrderByCodeAscending() {
			return EntityReader.getInstance().readMany(ScopeType.class, QUERY_IDENTIFIER_READ_ORDER_BY_CODE_ASCENDING);
		}
		
		@Override
		public Collection<ScopeType> readOrderByOrderNumberAscending() {
			return EntityReader.getInstance().readMany(ScopeType.class, QUERY_IDENTIFIER_READ_ORDER_BY_ORDER_NUMBER_ASCENDING);
		}
	}
	
	/**/
	
	static ScopeTypeQuerier getInstance() {
		return Helper.getInstance(ScopeTypeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	/**/
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(ScopeType.class);
		QueryHelper.addQueries(
			Query.buildSelect(ScopeType.class, QueryIdentifierGetter.getInstance().get(ScopeType.class, QueryName.READ)
				, "SELECT t FROM ScopeType t ORDER BY t.orderNumber ASC,t.code ASC")
			,Query.buildCount(QueryIdentifierGetter.getInstance().get(ScopeType.class, QueryName.COUNT), "SELECT COUNT(t.identifier) FROM ScopeType t")
		);
	}
}