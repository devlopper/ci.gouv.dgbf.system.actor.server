package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.EntityReader;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Profile;

public interface PrivilegeQuerier extends Querier {

	String PARAMETER_NAME_PROFILES_TYPES_CODES = "profilesTypesCodes";
	String PARAMETER_NAME_FUNCTIONS_CODES = "functionsCodes";
	
	/* read by profiles types codes by functions codes order by code ascending */
	String QUERY_NAME_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES_ORDER_BY_CODE_ASCENDING = "readByProfilesTypesCodesByFunctionsCodesOrderByCodeAscending";
	String QUERY_IDENTIFIER_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES_ORDER_BY_CODE_ASCENDING = QueryIdentifierBuilder.getInstance().build(Profile.class, QUERY_NAME_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES_ORDER_BY_CODE_ASCENDING);
	String QUERY_VALUE_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES_ORDER_BY_CODE_ASCENDING = Language.of(Language.Select.of("t")
			,Language.From.of("Privilege t"
					+ " JOIN ProfilePrivilege profilePrivilege ON profilePrivilege.privilege.identifier = t.identifier"
					+ " JOIN ProfileFunction profileFunction ON profileFunction.profile.identifier = profilePrivilege.profile.identifier"
					)			
			,Language.Where.of("profileFunction.profile.type.code IN :"+PARAMETER_NAME_PROFILES_TYPES_CODES+" AND profileFunction.function.code IN :"+PARAMETER_NAME_FUNCTIONS_CODES)			
			,Language.Order.of("t.code ASC"))
			;
	Collection<Privilege> readByProfilesTypesCodesByFunctionsCodesOrderByCodeAscending(Collection<String> typesCodes,Collection<String> functionsCodes);
	
	/**/
	
	public static abstract class AbstractImpl extends AbstractObject implements PrivilegeQuerier,Serializable {
		@Override
		public Collection<Privilege> readByProfilesTypesCodesByFunctionsCodesOrderByCodeAscending(Collection<String> profilesTypesCodes,Collection<String> functionsCodes) {
			return EntityReader.getInstance().readMany(Privilege.class, QUERY_IDENTIFIER_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES_ORDER_BY_CODE_ASCENDING
					, PARAMETER_NAME_PROFILES_TYPES_CODES,profilesTypesCodes,PARAMETER_NAME_FUNCTIONS_CODES,functionsCodes);
		}
	}
	
	/**/
	
	static PrivilegeQuerier getInstance() {
		return Helper.getInstance(PrivilegeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES_ORDER_BY_CODE_ASCENDING
				,Query.FIELD_TUPLE_CLASS,Privilege.class,Query.FIELD_RESULT_CLASS,Privilege.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_PROFILES_TYPES_CODES_BY_FUNCTIONS_CODES_ORDER_BY_CODE_ASCENDING
				)
			);
	}
}