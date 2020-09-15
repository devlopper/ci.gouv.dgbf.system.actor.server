package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.string.StringHelper;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public interface ScopeOfTypeQuerier extends Querier {

	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	String PARAMETER_NAME_ACTOR_CODE = "actorCode";
	String PARAMETER_NAME_ACTOR_CODE_NULLABLE = PARAMETER_NAME_ACTOR_CODE+"Nullable";
	
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME = 4;
	
	Collection<Scope> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	Collection<Scope> readWhereFilter(QueryExecutorArguments arguments);
	String getQueryIdentifierReadWhereFilter();
	static String buildReadWhereFilter(Class<?> klass) {
		return QueryIdentifierBuilder.getInstance().build(Scope.class, "read"+klass.getSimpleName()+"sWhereFilter");
	}
	
	Long countWhereFilter(QueryExecutorArguments arguments);
	String getQueryIdentifierCountWhereFilter();
	static String buildCountWhereFilter(Class<?> klass) {
		return QueryIdentifierBuilder.getInstance().buildCountFrom(buildReadWhereFilter(klass));
	}
	
	Collection<Scope> readVisibleWhereFilter(QueryExecutorArguments arguments);
	String getQueryIdentifierReadVisibleWhereFilter();
	static String buildReadVisibleWhereFilter(Class<?> klass) {
		return QueryIdentifierBuilder.getInstance().build(Scope.class, "readVisible"+klass.getSimpleName()+"sWhereFilter");
	}
	
	Long countVisibleWhereFilter(QueryExecutorArguments arguments);
	String getQueryIdentifierCountVisibleWhereFilter();
	static String buildCountVisibleWhereFilter(Class<?> klass) {
		return QueryIdentifierBuilder.getInstance().buildCountFrom(buildReadVisibleWhereFilter(klass));
	}
	
	Collection<Scope> readInvisibleWhereFilter(QueryExecutorArguments arguments);
	String getQueryIdentifierReadInvisibleWhereFilter();
	static String buildReadInvisibleWhereFilter(Class<?> klass) {
		return QueryIdentifierBuilder.getInstance().build(Scope.class, "readInvisible"+klass.getSimpleName()+"sWhereFilter");
	}
	
	Long countInvisibleWhereFilter(QueryExecutorArguments arguments);
	String getQueryIdentifierCountInvisibleWhereFilter();
	static String buildCountInvisibleWhereFilter(Class<?> klass) {
		return QueryIdentifierBuilder.getInstance().buildCountFrom(buildReadInvisibleWhereFilter(klass));
	}
	
	Boolean isVisible(String code,String actorCode);
	
	Boolean isInvisible(String code,String actorCode);
	
	Collection<String> getVisibles(Collection<String> codes,String actorCode);
	
	Collection<String> getInvisibles(Collection<String> codes,String actorCode);
	
	public static abstract class AbstractImpl extends AbstractObject implements ScopeOfTypeQuerier,Serializable {
		@Override
		public Collection<Scope> readMany(QueryExecutorArguments arguments) {
			if(getQueryIdentifierReadWhereFilter().equals(arguments.getQuery().getIdentifier()))
				return readWhereFilter(arguments);
			if(getQueryIdentifierReadVisibleWhereFilter().equals(arguments.getQuery().getIdentifier()))
				return readVisibleWhereFilter(arguments);
			if(getQueryIdentifierReadInvisibleWhereFilter().equals(arguments.getQuery().getIdentifier()))
				return readInvisibleWhereFilter(arguments);			
			throw new RuntimeException("Query <<"+arguments.getQuery().getIdentifier()+">> not readable by "+getClass());
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(getQueryIdentifierCountVisibleWhereFilter().equals(arguments.getQuery().getIdentifier()))
				return countVisibleWhereFilter(arguments);
			if(getQueryIdentifierCountInvisibleWhereFilter().equals(arguments.getQuery().getIdentifier()))
				return countInvisibleWhereFilter(arguments);
			if(getQueryIdentifierCountWhereFilter().equals(arguments.getQuery().getIdentifier()))
				return countWhereFilter(arguments);
			throw new RuntimeException("Query <<"+arguments.getQuery().getIdentifier()+">> not countable by "+getClass());
		}
		
		@Override
		public Collection<Scope> readVisibleWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(getQueryIdentifierReadVisibleWhereFilter());
			prepareVisibleWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countVisibleWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(getQueryIdentifierCountVisibleWhereFilter());
			prepareVisibleWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		protected void prepareVisibleWhereFilter(QueryExecutorArguments arguments) {
			prepareVisibleWhereFilterStatic(arguments);
		}
		
		public static void prepareVisibleWhereFilterStatic(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsEquals(arguments,PARAMETER_NAME_ACTOR_CODE);
			prepareVisibleWhereFilterAddFieldsCodeAndName(arguments, filter);
			arguments.setFilter(filter);
		}
		
		public static void prepareVisibleWhereFilterAddFieldsCodeAndName(QueryExecutorArguments arguments,Filter filter,String parameterNameCode,String parameterNameName) {
			filter.addFieldsContains(arguments,parameterNameCode);
			filter.addFieldContainsStringOrWords(parameterNameName, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
		}
		
		public static void prepareVisibleWhereFilterAddFieldsCodeAndName(QueryExecutorArguments arguments,Filter filter) {
			prepareVisibleWhereFilterAddFieldsCodeAndName(arguments, filter, PARAMETER_NAME_CODE, PARAMETER_NAME_NAME);
		}
		
		@Override
		public Collection<Scope> readInvisibleWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(getQueryIdentifierReadInvisibleWhereFilter());
			prepareInvisibleWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countInvisibleWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(getQueryIdentifierCountInvisibleWhereFilter());
			prepareInvisibleWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		protected void prepareInvisibleWhereFilter(QueryExecutorArguments arguments) {
			prepareInvisibleWhereFilterStatic(arguments);
		}
		
		public static void prepareInvisibleWhereFilterStatic(QueryExecutorArguments arguments) {
			prepareVisibleWhereFilterStatic(arguments);
		}
		
		@Override
		public Collection<Scope> readWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(getQueryIdentifierReadWhereFilter());
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(Scope.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(getQueryIdentifierCountWhereFilter());
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		protected void prepareWhereFilter(QueryExecutorArguments arguments) {
			prepareWhereFilterStatic(arguments);
		}
		
		public static void prepareWhereFilterStatic(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldsContains(arguments,PARAMETER_NAME_CODE);
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_NAME, arguments);
			arguments.setFilter(filter);
		}
	
		@Override
		public Boolean isVisible(String code,String actorCode) {
			if(StringHelper.isBlank(code))
				return null;
			return NumberHelper.isGreaterThanZero(countVisibleWhereFilter(new QueryExecutorArguments()
					.addFilterFieldsValues(PARAMETER_NAME_CODE,code,PARAMETER_NAME_ACTOR_CODE,actorCode)));
		}
		
		@Override
		public Boolean isInvisible(String code,String actorCode) {
			if(StringHelper.isBlank(code))
				return null;
			return NumberHelper.isGreaterThanZero(countInvisibleWhereFilter(new QueryExecutorArguments()
					.addFilterFieldsValues(PARAMETER_NAME_CODE,code,PARAMETER_NAME_ACTOR_CODE,actorCode)));
		}
		
		@Override
		public Collection<String> getVisibles(Collection<String> codes,String actorCode) {
			if(CollectionHelper.isEmpty(codes))
				return null;
			return codes.stream().filter(code -> isVisible(code,actorCode)).collect(Collectors.toList());
		}
		
		@Override
		public Collection<String> getInvisibles(Collection<String> codes,String actorCode) {
			if(CollectionHelper.isEmpty(codes))
				return null;
			return codes.stream().filter(code -> isInvisible(code,actorCode)).collect(Collectors.toList());
		}
	}
}