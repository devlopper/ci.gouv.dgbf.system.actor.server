package ci.gouv.dgbf.system.actor.server.persistence.api;

import java.io.Serializable;

import org.cyk.utility.__kernel__.persistence.query.CountQueryIdentifierGetter;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.FunctionQuerier;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class CountQueryIdentifierGetterImpl extends CountQueryIdentifierGetter.AbstractImpl implements Serializable {

	@Override
	protected String __get__(String readQueryIdentifier) {
		if(FunctionQuerier.QUERY_IDENTIFIER_READ_WITH_PROFILES_BY_TYPES_CODES.equals(readQueryIdentifier))
			return FunctionQuerier.QUERY_IDENTIFIER_COUNT_BY_TYPES_CODES;	
		return super.__get__(readQueryIdentifier);
	}
}