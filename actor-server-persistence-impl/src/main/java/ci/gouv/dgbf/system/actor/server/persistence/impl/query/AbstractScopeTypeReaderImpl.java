package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;

import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeType;

public abstract class AbstractScopeTypeReaderImpl extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<ScopeType> {

	@Override
	protected Class<ScopeType> getEntityClass() {
		return ScopeType.class;
	}
}