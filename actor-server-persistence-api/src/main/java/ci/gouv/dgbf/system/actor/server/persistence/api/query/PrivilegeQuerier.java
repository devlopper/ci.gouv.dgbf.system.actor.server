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

import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;

@Queries(value = {
		@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = Privilege.class,name = PrivilegeQuerier.QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING,value = "SELECT t FROM Privilege t ORDER BY t.code ASC")
})
public interface PrivilegeQuerier extends Querier {

	String PARAMETER_NAME_PROFILES_TYPES_CODES = "profilesTypesCodes";
	String PARAMETER_NAME_PROFILES_CODES = "profilesCodes";
	String PARAMETER_NAME_FUNCTIONS_CODES = "functionsCodes";
	String PARAMETER_NAME_ACTORS_CODES = "actorsCodes";
	
	/* read order by code ascending */
	String QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING = "readOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ_ORDER_BY_CODE_ASCENDING = QueryIdentifierBuilder.getInstance().build(Privilege.class, QUERY_NAME_READ_ORDER_BY_CODE_ASCENDING);
	
	/* read by profiles types codes by functions codes order by code ascending */
	String QUERY_NAME_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES = "readByProfilesTypesCodesByFunctionsCodes";
	String QUERY_IDENTIFIER_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES = QueryIdentifierBuilder.getInstance().build(Privilege.class, QUERY_NAME_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES);
	String QUERY_VALUE_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Privilege t"
					+ " JOIN ProfilePrivilege profilePrivilege ON profilePrivilege.privilege.identifier = t.identifier"
					+ " JOIN ProfileFunction profileFunction ON profileFunction.profile.identifier = profilePrivilege.profile.identifier"
					)			
			,Language.Where.of("profileFunction.profile.type.code IN :"+PARAMETER_NAME_PROFILES_TYPES_CODES+" AND profileFunction.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)			
			,Language.Order.of("t.code ASC"))
			;
	Collection<Privilege> readByProfilesTypesCodesByFunctionsCodes(Collection<String> typesCodes,Collection<String> functionsCodes);
	
	/* read by actors codes order by code ascending */
	String QUERY_NAME_READ_BY_ACTORS_CODES = "readByActorsCodes";
	String QUERY_IDENTIFIER_READ_BY_ACTORS_CODES = QueryIdentifierBuilder.getInstance().build(Privilege.class, QUERY_NAME_READ_BY_ACTORS_CODES);
	String QUERY_VALUE_READ_BY_ACTORS_CODES = Language.of(Language.Select.of("t")
			,Language.From.of("Privilege t"
					+ " JOIN ProfilePrivilege profilePrivilege ON profilePrivilege.privilege.identifier = t.identifier"
					+ " JOIN ActorProfile actorProfile ON actorProfile.profile.identifier = profilePrivilege.profile.identifier"
					)			
			,Language.Where.of("actorProfile.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES)			
			,Language.Order.of("t.code ASC"))
			;
	Collection<Privilege> readByActorsCodes(Collection<String> actorsCodes);
	
	/* count by actors codes order by code ascending */
	String QUERY_NAME_COUNT_BY_ACTORS_CODES = "countByActorsCodes";
	String QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES = QueryIdentifierBuilder.getInstance().build(Privilege.class, QUERY_NAME_COUNT_BY_ACTORS_CODES);
	String QUERY_VALUE_COUNT_BY_ACTORS_CODES = Language.of(Language.Select.of("COUNT(t.identifier)")
			,Language.From.of("Privilege t"
					+ " JOIN ProfilePrivilege profilePrivilege ON profilePrivilege.privilege.identifier = t.identifier"
					+ " JOIN ActorProfile actorProfile ON actorProfile.profile.identifier = profilePrivilege.profile.identifier"
					)			
			,Language.Where.of("actorProfile.actor.code IN :"+PARAMETER_NAME_ACTORS_CODES)			
			);
	Long countByActorsCodes(Collection<String> actorsCodes);
	
	/* read by profiles codes not associated order by code ascending */
	String QUERY_NAME_READ_BY_PROFILES_CODES_NOT_ASSOCIATED = "readByProfilesCodesNotAssociated";
	String QUERY_IDENTIFIER_READ_BY_PROFILES_CODES_NOT_ASSOCIATED = QueryIdentifierBuilder.getInstance().build(Privilege.class, QUERY_NAME_READ_BY_PROFILES_CODES_NOT_ASSOCIATED);
	String QUERY_VALUE_READ_BY_PROFILES_CODES_NOT_ASSOCIATED = Language.of(Language.Select.of("t")
			,Language.From.of("Privilege t")			
			,Language.Where.of("NOT EXISTS(SELECT pp FROM ProfilePrivilege pp WHERE pp.privilege.identifier = t.identifier AND pp.profile.code IN :"+PARAMETER_NAME_PROFILES_CODES+")")			
			,Language.Order.of("t.code ASC"))
			;
	Collection<Privilege> readByProfilesCodesNotAssociated(Collection<String> profilesCodes);
	
	/* count by profiles codes not associated */
	String QUERY_NAME_COUNT_BY_PROFILES_CODES_NOT_ASSOCIATED = "countByProfilesCodesNotAssociated";
	String QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES_NOT_ASSOCIATED = QueryIdentifierBuilder.getInstance().build(Privilege.class, QUERY_NAME_COUNT_BY_PROFILES_CODES_NOT_ASSOCIATED);
	String QUERY_VALUE_COUNT_BY_PROFILES_CODES_NOT_ASSOCIATED = Language.of(Language.Select.of("COUNT(t.identifier)")
			,Language.From.of("Privilege t")			
			,Language.Where.of("NOT EXISTS(SELECT pp FROM ProfilePrivilege pp WHERE pp.privilege.identifier = t.identifier AND pp.profile.code IN :"+PARAMETER_NAME_PROFILES_CODES+")")			
			);
	Long countByProfilesCodesNotAssociated(Collection<String> profilesCodes);
	
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements PrivilegeQuerier,Serializable {
		@Override
		public Collection<Privilege> readByProfilesTypesCodesByFunctionsCodes(Collection<String> profilesTypesCodes,Collection<String> functionsCodes) {
			return EntityReader.getInstance().readMany(Privilege.class, QUERY_IDENTIFIER_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES
					, PARAMETER_NAME_PROFILES_TYPES_CODES,profilesTypesCodes,PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
		
		@Override
		public Collection<Privilege> readByActorsCodes(Collection<String> actorsCodes) {
			return EntityReader.getInstance().readMany(Privilege.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES, PARAMETER_NAME_ACTORS_CODES,actorsCodes);
		}
		
		@Override
		public Long countByActorsCodes(Collection<String> actorsCodes) {
			return EntityCounter.getInstance().count(Privilege.class, QUERY_IDENTIFIER_READ_BY_ACTORS_CODES, PARAMETER_NAME_ACTORS_CODES,actorsCodes);
		}
		
		@Override
		public Collection<Privilege> readByProfilesCodesNotAssociated(Collection<String> profilesCodes) {
			return EntityReader.getInstance().readMany(Privilege.class, QUERY_IDENTIFIER_READ_BY_PROFILES_CODES_NOT_ASSOCIATED, PARAMETER_NAME_PROFILES_CODES,profilesCodes);
		}
		
		@Override
		public Long countByProfilesCodesNotAssociated(Collection<String> profilesCodes) {
			return EntityCounter.getInstance().count(Privilege.class, QUERY_IDENTIFIER_READ_BY_PROFILES_CODES_NOT_ASSOCIATED, PARAMETER_NAME_PROFILES_CODES,profilesCodes);
		}
	}
	
	/**/
	
	static PrivilegeQuerier getInstance() {
		return Helper.getInstance(PrivilegeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Privilege.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACTORS_CODES
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Privilege.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ACTORS_CODES
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_ACTORS_CODES
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_ACTORS_CODES
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_PROFILES_CODES_NOT_ASSOCIATED
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Privilege.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_PROFILES_CODES_NOT_ASSOCIATED
				)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_BY_PROFILES_CODES_NOT_ASSOCIATED
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_BY_PROFILES_CODES_NOT_ASSOCIATED
				)
			);
	}
}