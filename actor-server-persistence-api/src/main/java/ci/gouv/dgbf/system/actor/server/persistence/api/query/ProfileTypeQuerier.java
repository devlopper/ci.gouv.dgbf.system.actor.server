package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.annotation.Queries;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.persistence.server.query.executor.DynamicOneExecutor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ProfileType;

@Queries(value = {
		@org.cyk.utility.persistence.annotation.Query(tupleClass = ProfileType.class,name = ProfileTypeQuerier.QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING,value = "SELECT t FROM ProfileType t ORDER BY t.code ASC")
})
public interface ProfileTypeQuerier extends Querier.CodableAndNamable<ProfileType> {

	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(ProfileType.class, QueryName.READ_DYNAMIC);	
	String QUERY_IDENTIFIER_READ_DYNAMIC_ONE = QueryIdentifierBuilder.getInstance().build(ProfileType.class, QueryName.READ_DYNAMIC_ONE);
	String QUERY_IDENTIFIER_COUNT_DYNAMIC = QueryIdentifierBuilder.getInstance().build(ProfileType.class, QueryName.COUNT_DYNAMIC);
	
	/* read order by code ascending */
	String QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ_ORDER_BY_CODE_ASCENDING = QueryIdentifierBuilder.getInstance().build(ProfileType.class, QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING);
	Collection<ProfileType> readOrderByCodeAscending();
		
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<ProfileType> implements ProfileTypeQuerier,Serializable {	
		@Override
		public Collection<ProfileType> readOrderByCodeAscending() {
			return QueryExecutor.getInstance().executeReadMany(ProfileType.class, QUERY_IDENTIFIER_READ_ORDER_BY_CODE_ASCENDING);
		}
		
		@Override
		public ProfileType readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC_ONE.equals(arguments.getQuery().getIdentifier()))
				return DynamicOneExecutor.getInstance().read(ProfileType.class,arguments.setQuery(null));
			return super.readOne(arguments);
		}
		
		@Override
		public Collection<ProfileType> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_ORDER_BY_CODE_ASCENDING.equals(arguments.getQuery().getIdentifier()))
				return readOrderByCodeAscending();
			if(QUERY_IDENTIFIER_READ_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().read(ProfileType.class,arguments.setQuery(null));
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().count(ProfileType.class,arguments.setQuery(null));
			return super.count(arguments);
		}
		
		@Override
		protected Class<ProfileType> getKlass() {
			return ProfileType.class;
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