package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.__kernel__.persistence.query.Language.jpql;
import static org.cyk.utility.__kernel__.persistence.query.Language.From.from;
import static org.cyk.utility.__kernel__.persistence.query.Language.Order.order;
import static org.cyk.utility.__kernel__.persistence.query.Language.Select.select;
import static org.cyk.utility.__kernel__.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Language.Where;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Actor;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;

@Queries(value = {
		@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Profile.class,name = ProfileQuerier.QUERY_NAME_READ_WHERE_TYPE_IS_SYSTEME
				,value = "SELECT t FROM Profile t WHERE t.type.code = 'SYSTEME' ORDER BY t.code ASC")
		,@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Profile.class,name = ProfileQuerier.QUERY_NAME_READ
				,value = "SELECT t FROM Profile t ORDER BY t.code ASC")
})
public interface ProfileQuerier extends Querier.CodableAndNamable<Profile> {

	String PARAMETER_NAME_TYPES_CODES = "typesCodes";
	String PARAMETER_NAME_FUNCTIONS_CODES = "functionsCodes";
	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	
	/* read order by code ascending */
	String QUERY_NAME_READ_WHERE_TYPE_IS_SYSTEME = "readWhereTypeIsSystemeOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_SYSTEME = QueryIdentifierBuilder.getInstance().build(Profile.class, QUERY_NAME_READ_WHERE_TYPE_IS_SYSTEME);
	Collection<Profile> readWhereTypeIsSystemeOrderByCodeAscending();
	
	/* read order by code ascending */
	String QUERY_NAME_READ = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(Profile.class, QUERY_NAME_READ);
	Collection<Profile> read();
	
	/* read by types codes by functions codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().build(Profile.class, "readByTypesCodesByFunctionsCodes");
	Collection<Profile> readByTypesCodesByFunctionsCodes(Collection<String> typesCodes,Collection<String> functionsCodes);
	
	/* read by types codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Profile.class, "readByTypesCodes");
	Collection<Profile> readByTypesCodes(Collection<String> typesCodes);
	
	/* count by types codes */
	String QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_TYPES_CODES);
	Long countByTypesCodes(Collection<String> typesCodes);
	
	/* read by actors codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES = QueryIdentifierBuilder.getInstance().build(Profile.class, "readByActorsCodes");
	Collection<Profile> readByActorsCodes(Collection<String> actorsCodes);
	
	/* read by functions codes order by code ascending */
	String QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().build(Profile.class, "readByFunctionsCodes");
	Collection<Profile> readByFunctionsCodes(Collection<String> functionsCodes);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Profile> implements ProfileQuerier,Serializable {
		@Override
		public Collection<Profile> readWhereTypeIsSystemeOrderByCodeAscending() {
			return QueryExecutor.getInstance().executeReadMany(Profile.class, QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_SYSTEME);
		}
		
		@Override
		public Collection<Profile> read() {
			return QueryExecutor.getInstance().executeReadMany(Profile.class, QUERY_IDENTIFIER_READ);
		}
		
		@Override
		public Collection<Profile> readByTypesCodesByFunctionsCodes(Collection<String> typesCodes,Collection<String> functionsCodes) {
			return QueryExecutor.getInstance().executeReadMany(Profile.class, QUERY_IDENTIFIER_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES
					, PARAMETER_NAME_TYPES_CODES,typesCodes,PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
		
		@Override
		public Collection<Profile> readByTypesCodes(Collection<String> typesCodes) {
			return QueryExecutor.getInstance().executeReadMany(Profile.class, QUERY_IDENTIFIER_READ_BY_TYPES_CODES, PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Long countByTypesCodes(Collection<String> typesCodes) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES,PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Collection<Profile> readByActorsCodes(Collection<String> actorsCodes) {
			return QueryExecutor.getInstance().executeReadMany(Profile.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES, PARAMETER_NAME_ACTORS_CODES,actorsCodes);
		}
		
		@Override
		public Collection<Profile> readByFunctionsCodes(Collection<String> functionsCodes) {
			return QueryExecutor.getInstance().executeReadMany(Profile.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES, PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public Collection<Profile> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ.equals(arguments.getQuery().getIdentifier()))
				return read();
			if(QUERY_IDENTIFIER_READ_BY_TYPES_CODES.equals(arguments.getQuery().getIdentifier()))
				return readByTypesCodes((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_TYPES_CODES));
			if(QUERY_IDENTIFIER_READ_BY_ACTORS_CODES.equals(arguments.getQuery().getIdentifier()))
				return readByActorsCodes((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_ACTORS_CODES));
			if(QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES.equals(arguments.getQuery().getIdentifier()))
				return readByFunctionsCodes((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_FUNCTIONS_CODES));
			if(QUERY_IDENTIFIER_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES.equals(arguments.getQuery().getIdentifier()))
				return readByTypesCodesByFunctionsCodes((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_TYPES_CODES)
						,(Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_FUNCTIONS_CODES));		
			return super.readMany(arguments);
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES.equals(arguments.getQuery().getIdentifier()))
				return countByTypesCodes((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_TYPES_CODES));			
			return super.count(arguments);
		}
		
		@Override
		protected Class<Profile> getKlass() {
			return Profile.class;
		}
	}
	
	/**/
	
	static ProfileQuerier getInstance() {
		return Helper.getInstance(ProfileQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(Profile.class);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Profile.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("t")
						,Language.From.of("Profile t JOIN ProfileFunction profileFunction ON profileFunction.profile.identifier = t.identifier")			
						,Language.Where.of("profileFunction.profile.type.code IN :"+PARAMETER_NAME_TYPES_CODES+" AND profileFunction.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)			
						,Language.Order.of("t.code ASC"))
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Profile.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("t")
						,Language.From.of("Profile t")			
						,Language.Where.of("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES)
						,Language.Order.of("t.code ASC"))
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,Language.of(Language.Select.of("COUNT(t.identifier)")
						,Language.From.of("Profile t")			
						,Language.Where.of("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES))
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTORS_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Profile.class
				,Query.FIELD_VALUE,jpql(select("t")
						,from("Profile t JOIN ActorProfile actorProfile ON actorProfile.profile = t")			
						,where(Where.in("actorProfile.actor", Actor.FIELD_CODE, PARAMETER_NAME_ACTORS_CODES))			
						,order("t.code ASC"))
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Profile.class
				,Query.FIELD_VALUE,jpql(select("t")
						,from("Profile t JOIN ProfileFunction profileFunction ON profileFunction.profile.identifier = t.identifier")			
						,where("profileFunction.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)			
						,order("t.code ASC"))
				)
			);
	}
}