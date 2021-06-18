package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import org.cyk.utility.persistence.server.query.ArraysReaderByIdentifiers;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Assignments;

public abstract class AbstractAssignmentsReaderImpl extends ArraysReaderByIdentifiers.AbstractImpl.DefaultImpl<Assignments> {

	@Override
	protected Class<Assignments> getEntityClass() {
		return Assignments.class;
	}
}