package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ActivityCategory;

@Queries(value = {
	@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = ActivityCategory.class,name = ActivityCategoryQuerier.QUERY_NAME_READ,value = "SELECT t FROM ActivityCategory t ORDER BY t.code ASC")
	//,@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Function.class,name = FunctionQuerier.QUERY_NAME_COUNT,value = "SELECT COUNT(t.identifier) FROM Function t")
})
public interface ActivityCategoryQuerier extends Querier.CodableAndNamable<ActivityCategory> {

	/* read order by code ascending */
	String QUERY_NAME_READ = "read";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(ActivityCategory.class, QUERY_NAME_READ);
	Collection<ActivityCategory> read();
	
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ);
	Long count();
	
	String QUERY_IDENTIFIER_READ_ALL_FOR_UI = QueryIdentifierBuilder.getInstance().build(ActivityCategory.class, "readAllForUI");
	Collection<ActivityCategory> readAllForUI();
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<ActivityCategory> implements ActivityCategoryQuerier,Serializable {
		@Override
		public Collection<ActivityCategory> read() {
			return QueryExecutor.getInstance().executeReadMany(ActivityCategory.class, QUERY_IDENTIFIER_READ);
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT);
		}
		
		@Override
		public Collection<ActivityCategory> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ.equals(arguments.getQuery().getIdentifier()))
				return read();
			if(QUERY_IDENTIFIER_READ_ALL_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readAllForUI();
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT.equals(arguments.getQuery().getIdentifier()))
				return count();
			return super.count(arguments);
		}
		
		@Override
		public Collection<ActivityCategory> readAllForUI() {
			return QueryExecutor.getInstance().executeReadMany(ActivityCategory.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI);
		}
		
		@Override
		protected Class<ActivityCategory> getKlass() {
			return ActivityCategory.class;
		}
	}
	
	/**/
	
	static ActivityCategoryQuerier getInstance() {
		return Helper.getInstance(ActivityCategoryQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(ActivityCategory.class);
		QueryHelper.addQueries(Query.buildSelect(ActivityCategory.class, QUERY_IDENTIFIER_READ_ALL_FOR_UI
				, "SELECT t.identifier,t.code,t.name FROM ActivityCategory t ORDER BY t.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(ActivityCategory.FIELD_IDENTIFIER,ActivityCategory.FIELD_CODE,ActivityCategory.FIELD_NAME));
	}
}