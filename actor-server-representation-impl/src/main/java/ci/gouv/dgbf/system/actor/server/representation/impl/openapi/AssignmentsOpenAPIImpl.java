package ci.gouv.dgbf.system.actor.server.representation.impl.openapi;

import java.io.Serializable;
import java.util.List;

import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.string.StringHelper;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.representation.impl.AssignmentsRepresentationImpl;
import ci.gouv.dgbf.system.actor.server.representation.impl.ScopeFunctionRepresentationImpl;

public class AssignmentsOpenAPIImpl extends AbstractOpenAPIImpl implements AssignmentsOpenAPI,Serializable {

	@Override
	public Response createByScopeIdentifierByCategoryCode(String scopeIdentifier,String categoryCode,String name, String actorCode) {
		if(StringHelper.isNotBlank(scopeIdentifier) && StringHelper.isNotBlank(categoryCode)) {
			String scopeTypeCode = ScopeFunction.getScopeTypeCodeFromCategoryCode(categoryCode);
			if(StringHelper.isNotBlank(scopeTypeCode))
				scopeIdentifier = scopeTypeCode+scopeIdentifier;
		}
		return ScopeFunctionRepresentationImpl.createByScopeIdentifierByCategoryCodeStatic(scopeIdentifier,categoryCode,name,actorCode,Boolean.TRUE);
	}
	
	@Override
	public Response importNewsAndDeriveValuesByIdentifiersAndExport(List<String> identifiers, String actorCode) {
		return AssignmentsRepresentationImpl.importNewsAndDeriveValuesByIdentifiersAndExport(identifiers, actorCode);
	}
}