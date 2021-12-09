package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;

public abstract class AbstractRequestScopeFunctionReaderImpl extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<RequestScopeFunction> {

	@Override
	protected Class<RequestScopeFunction> getEntityClass() {
		return RequestScopeFunction.class;
	}
}