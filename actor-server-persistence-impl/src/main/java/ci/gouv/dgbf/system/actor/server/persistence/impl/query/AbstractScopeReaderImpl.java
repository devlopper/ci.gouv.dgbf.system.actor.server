package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public abstract class AbstractScopeReaderImpl extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<Scope> {

	@Override
	protected Class<Scope> getEntityClass() {
		return Scope.class;
	}
}