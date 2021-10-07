package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;
import java.util.Collection;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

public class RequestGrantedScopeFunctionsCodesReader extends RequestScopeFunctionsCodesReader implements Serializable {

	@Override
	protected Collection<String> instantiateQueryStringBuilderArgumentsPredicates() {
		Collection<String> collection = super.instantiateQueryStringBuilderArgumentsPredicates();
		collection.add("rsf.granted = true");
		return collection;
	}

	@Override
	protected void __set__(Request request, Object[] array) {
		Integer index = 1;
		request.getGrantedScopeFunctionsCodes(Boolean.TRUE).add(getAsString(array, index));
	}
	
	@Override
	protected Boolean isEntityHasOnlyArray(Request request) {
		return Boolean.FALSE;
	}
}