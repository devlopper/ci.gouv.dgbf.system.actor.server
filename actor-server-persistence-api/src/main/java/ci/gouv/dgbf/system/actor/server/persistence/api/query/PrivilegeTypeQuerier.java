package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;

@Queries(value = {
	@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = PrivilegeType.class,name = PrivilegeTypeQuerier.QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING,value = "SELECT t FROM PrivilegeType t ORDER BY t.code ASC")
	,@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = PrivilegeType.class,name = PrivilegeTypeQuerier.QUERY_NAME_READ_ORDER_BY_ORDER_NUMBER_ASCENDING,value = "SELECT t FROM PrivilegeType t ORDER BY t.orderNumber ASC")
})
public interface PrivilegeTypeQuerier extends Querier {

	/* read order by order number */
	String QUERY_NAME_READ_ORDER_BY_ORDER_NUMBER_ASCENDING = "readOrderByOrderNumberAscending";
	String QUERY_IDENTIFIER_READ_ORDER_BY_ORDER_NUMBER_ASCENDING = QueryIdentifierBuilder.getInstance().build(PrivilegeType.class, QUERY_NAME_READ_ORDER_BY_ORDER_NUMBER_ASCENDING);
	
	/* read order by code ascending */
	String QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ_ORDER_BY_CODE_ASCENDING = QueryIdentifierBuilder.getInstance().build(PrivilegeType.class, QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING);
	Collection<PrivilegeType> readOrderByCodeAscending();
		
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements PrivilegeTypeQuerier,Serializable {	
		@Override
		public Collection<PrivilegeType> readOrderByCodeAscending() {
			return EntityReader.getInstance().readMany(PrivilegeType.class, QUERY_IDENTIFIER_READ_ORDER_BY_CODE_ASCENDING);
		}
	}
	
	/**/
	
	static PrivilegeTypeQuerier getInstance() {
		return Helper.getInstance(PrivilegeTypeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	/**/
	
	static void initialize() {
		
	}
}