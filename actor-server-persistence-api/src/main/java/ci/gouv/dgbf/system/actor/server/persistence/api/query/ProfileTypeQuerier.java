package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.server.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

@Queries(value = {
		@org.cyk.utility.persistence.server.annotation.Query(tupleClass = ProfileType.class,name = ProfileTypeQuerier.QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING,value = "SELECT t FROM ProfileType t ORDER BY t.code ASC")
})
public interface ProfileTypeQuerier extends Querier {

	/* read order by code ascending */
	String QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ_ORDER_BY_CODE_ASCENDING = QueryIdentifierBuilder.getInstance().build(ProfileType.class, QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING);
	Collection<ProfileType> readOrderByCodeAscending();
		
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements ProfileTypeQuerier,Serializable {	
		@Override
		public Collection<ProfileType> readOrderByCodeAscending() {
			return EntityReader.getInstance().readMany(ProfileType.class, QUERY_IDENTIFIER_READ_ORDER_BY_CODE_ASCENDING);
		}
	}
	
	/**/
	
	static ProfileTypeQuerier getInstance() {
		return Helper.getInstance(ProfileTypeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	/**/
	
	static void initialize() {
		
	}
}