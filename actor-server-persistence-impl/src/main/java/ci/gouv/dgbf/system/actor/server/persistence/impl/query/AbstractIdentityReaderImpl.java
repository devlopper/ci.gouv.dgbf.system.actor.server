package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;

public abstract class AbstractIdentityReaderImpl extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<Identity> {

	@Override
	protected Class<Identity> getEntityClass() {
		return Identity.class;
	}
}