package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.EntityCounter;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;

@Queries(value = {
		@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Profile.class,name = ProfileQuerier.QUERY_NAME_READ_WHERE_TYPE_IS_SYSTEME
				,value = "SELECT t FROM Profile t WHERE t.type.code = 'SYSTEME' ORDER BY t.code ASC")
})
public interface ProfileQuerier extends Querier {

	String PARAMETER_NAME_TYPES_CODES = "typesCodes";
	String PARAMETER_NAME_FUNCTIONS_CODES = "functionsCodes";
	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	
	/* read order by code ascending */
	String QUERY_NAME_READ_WHERE_TYPE_IS_SYSTEME = "readWhereTypeIsSystemeOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ_WHERE_TYPE_IS_SYSTEME = QueryIdentifierBuilder.getInstance().build(Profile.class, QUERY_NAME_READ_WHERE_TYPE_IS_SYSTEME);
	
	/* read by types codes by functions codes order by code ascending */
	String QUERY_NAME_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES = "readByTypesCodesByFunctionsCodes";
	String QUERY_IDENTIFIER_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().build(Profile.class, QUERY_NAME_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES);
	String QUERY_VALUE_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Profile t JOIN ProfileFunction profileFunction ON profileFunction.profile.identifier = t.identifier")			
			,Language.Where.of("profileFunction.profile.type.code IN :"+PARAMETER_NAME_TYPES_CODES+" AND profileFunction.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)			
			,Language.Order.of("t.code ASC"))
			;
	Collection<Profile> readByTypesCodesByFunctionsCodes(Collection<String> typesCodes,Collection<String> functionsCodes);
	
	/* read by types codes order by code ascending */
	String QUERY_NAME_READ_BY_TYPES_CODES = "readByTypesCodes";
	String QUERY_IDENTIFIER_READ_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Profile.class, QUERY_NAME_READ_BY_TYPES_CODES);
	String QUERY_VALUE_READ_BY_TYPES_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Profile t")			
			,Language.Where.of("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES)
			,Language.Order.of("t.code ASC"))
			;
	Collection<Profile> readByTypesCodes(Collection<String> typesCodes);
	
	/* count by types codes */
	String QUERY_NAME_COUNT_BY_TYPES_CODES = "countByTypesCodes";
	String QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES = QueryIdentifierBuilder.getInstance().build(Profile.class, QUERY_NAME_COUNT_BY_TYPES_CODES);
	String QUERY_VALUE_COUNT_BY_TYPES_CODES = Language.of(Language.Select.of("COUNT(t.identifier)")
			,Language.From.of("Profile t")			
			,Language.Where.of("t.type.code IN :"+PARAMETER_NAME_TYPES_CODES))
			;
	Long countByTypesCodes(Collection<String> typesCodes);
	
	/* read by actors codes order by code ascending */
	String QUERY_NAME_READ_BY_ACTORS_CODES = "readByActorsCodes";
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES = QueryIdentifierBuilder.getInstance().build(Profile.class, QUERY_NAME_READ_BY_ACTORS_CODES);
	String QUERY_VALUE_READ_BY_ACTORS_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Profile t JOIN ActorProfile actorProfile ON actorProfile.profile.identifier = t.identifier")			
			,Language.Where.of("actorProfile.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES)			
			,Language.Order.of("t.code ASC"))
			;
	Collection<Profile> readByActorsCodes(Collection<String> actorsCodes);
	
	/* read by functions codes order by code ascending */
	String QUERY_NAME_READ_BY_FUNCTIONS_CODES = "readByFunctionsCodes";
	String QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().build(Profile.class, QUERY_NAME_READ_BY_FUNCTIONS_CODES);
	String QUERY_VALUE_READ_BY_FUNCTIONS_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Profile t JOIN ProfileFunction profileFunction ON profileFunction.profile.identifier = t.identifier")			
			,Language.Where.of("profileFunction.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)			
			,Language.Order.of("t.code ASC"))
			;
	Collection<Profile> readByFunctionsCodes(Collection<String> functionsCodes);
	
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements ProfileQuerier,Serializable {
		@Override
		public Collection<Profile> readByTypesCodesByFunctionsCodes(Collection<String> typesCodes,Collection<String> functionsCodes) {
			return EntityReader.getInstance().readMany(Profile.class, QUERY_IDENTIFIER_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES
					, PARAMETER_NAME_TYPES_CODES,typesCodes,PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
		
		@Override
		public Collection<Profile> readByTypesCodes(Collection<String> typesCodes) {
			return EntityReader.getInstance().readMany(Profile.class, QUERY_IDENTIFIER_READ_BY_TYPES_CODES, PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Long countByTypesCodes(Collection<String> typesCodes) {
			return EntityCounter.getInstance().count(Profile.class, QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES,PARAMETER_NAME_TYPES_CODES,typesCodes);
		}
		
		@Override
		public Collection<Profile> readByActorsCodes(Collection<String> actorsCodes) {
			return EntityReader.getInstance().readMany(Profile.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES, PARAMETER_NAME_ACTORS_CODES,actorsCodes);
		}
		
		@Override
		public Collection<Profile> readByFunctionsCodes(Collection<String> functionsCodes) {
			return EntityReader.getInstance().readMany(Profile.class, QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES, PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
	}
	
	/**/
	
	static ProfileQuerier getInstance() {
		return Helper.getInstance(ProfileQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Profile.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_TYPES_CODES_BY_FUNCTIONS_CODES
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Profile.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_TYPES_CODES
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_TYPES_CODES
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTORS_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Profile.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ACTORS_CODES
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,Profile.class,Query.FIELD_RESULT_CLASS,Profile.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_FUNCTIONS_CODES
				)
			);
	}
}